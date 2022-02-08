
-- Can discuss skipping SockAdrr completely, at least in this combiner
-- also seperating types for different sets of answers

data MessageHandler = MessageHandler (Message -> IO Answer)
data MessageHandler msg answer = MessageHandler (msg -> IO ans)


combineHandlers :: 
    (appState -> MessageHandler () PingAnswer)  -- react to PingMessage
    -> (appState -> MessageHandler Block BlockAnswer) -- react to BlockMessage
    -> (appState -> MessageHandler Transaction TransactionAnswer)
    -> (appState -> MessageHandler BlocksQuery BlocksAnswer)
    -> (appState -> MessageHandler ContactQuery ContactAnswer)
    -> MessageHandler 

combineHandlers :: 
    (appState -> IO Answer)  -- react to PingMessage
    -> (appState -> Block -> IO Answer) -- react to BlockMessage
    -> (appState -> Transaction -> IO Answer)
    -> (appState -> Query -> IO Answer)
    -> (appState -> IO Answer)
    -> MessageHandler

-- type ServerHandler = HandlerFunc -- better name


toServerHandler :: MessageHandler
                -> (String -> IO ())  -- what to do on parsing error, (String -> IO() / IO())
                -> ServerHandler
toServerHandler (MessageHandler handler) log _ msgbytes = do
    case decode msgbytes of 
        Nothing -> do 
            log "handler: Unable to parse message."
            return $ encode MessageParseError
        Just msg -> do
            answer <-  handler msg
            return $ encode answer

serverHandler :: (SockAddr -> Message -> IO Answer) -> (String -> IO ()) -> HandlerFunc
serverHandler handler log sockAddr msgbytes = do
    case decode msgbytes of 
        Nothing -> do 
            log "handler: Unable to parse message."
            return $ encode MessageParseError
        Just msg -> do
            answer <-  handler sockAddr msg
            return $ encode answer


handleMessage :: (HasPeers appState, PureUpdateWithBlock s r, AtomicModify s appState IO)
              => ForkMaxDiff
              -> TargetHash
              -> appState
              -> (String -> IO ())
              -> SockAddr
              -> Message
              -> IO Answer
handleMessage _ _ appSt log sockaddr PingMessage = do
    log "handler: Received Ping."
    return PingAnswer

handleMessage forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) action log sockaddr (BlockMessage block) =
    genericHandleBlock action forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) log sockaddr block

handleMessage _ _ (AppState {blockchainState}) log sockaddr (TransactionMessage tx) = do 
    -- 2 options, one to ignore
    receiveTransaction blockchainState log sockaddr tx

handleMessage _ _ (AppState {blockchainState}) log sockaddr (BlockchainQuery query) = 
    answerQuery blockchainState log sockaddr query

-- Answer a query with all the contacts from out contact list.  
handleMessage _ _ (AppState {peers}) log sockaddr ContactQuery = do    
    -- Here maybe 2 options? one to ignore
    log "handler: Received a contact query."
    peers <- readTVarIO peers 
    return (ContactQueryAnswer $ getAddresses peers)


genericHandleBlock  :: 
    (HasPeers appState, PureUpdateWithBlock s r, AtomicModify s appState IO)
              => ForkMaxDiff
              -> TargetHash
              -> appState
              -> (r -> IO ())
              -> (String -> IO ())
              -> SockAddr
              -> Message
              -> IO Answer
genericHandleBlock forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) action log sockaddr block= do    
    -- do the validation etc concurrently not to hold a connection for long
    forkIO $ do
            r <- atomicModify appState (
                let (r, s) = pureUpdate block
                in (r,s) )  -- function returns pair)
            action r
                            
    return ReceivedBlock


answerQuery blockchainState log sockaddr query = 
    log "handler: Received a blockchain query."
    case query of
        -- This is pretty tragic as is indexing in a linked list
        BlockAtHeight n -> do
            blocks <- atomically $ readFixedBlocks blockchainState
            case blocks of
                FixedBlocks [] -> return (BlockchainQueryAnswer NoBlockFound) 
                FixedBlocks (b : bs) ->
                    if blockBlockHeight b >= n then
                        case find (\x -> blockBlockHeight x == n) (b:bs) of
                            Just b -> do return (BlockchainQueryAnswer (RequestedBlock b))
                            Nothing -> return (BlockchainQueryAnswer NoBlockFound)
                    else
                        return (BlockchainQueryAnswer NoBlockFound)


newtype TransactionQueue = TransactionQueue (Seq.Seq Transaction)

receiveTransaction :: (HasLogging appState, InMemoryRead appState UTXOPool, InMemory appState TransactionQueue) =>
    appState -> MsgHandler Transaction ReceivedTransaction
receiveTransaction appState tx = do
    -- append new transaction to queue
    -- Transactions are initially validated against utxo's from known FixedBlocks.
    -- TODO: This (maybe) should be updated to validate for state at the tip of LivelyBlocks.

    utxoPool <- readMemoryIO appState
    if validTransaction utxoPool tx then do
        logger appState "handler: Received new transaction."        
        runAtomically $ modifyMemory appState (`Seq.|>` tx)
    else
        logger appState "handler: Received new transaction."

    return ReceivedTransaction

ignoreTransaction :: MsgHandler Transaction ReceivedTransaction
ignoreTransaction = MsgHandler $ const (return ReceivedTransaction)