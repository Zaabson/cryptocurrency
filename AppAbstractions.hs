module AppAbstractions where 
import Server (server, Address)
import App (serverHandler, BlockchainState (BlockchainState))
import Message (Message)

handleMessage :: ForkMaxDiff
              -> TargetHash
              -> AppState
              -> (String -> IO ())
              -> SockAddr
              -> Message
              -> IO Answer

fullNodeHandleMessage :: (HasLogging m, HasAppState m) =>
                 ForkMaxDiff
              -> TargetHash
              -> SockAddr
              -> Message
              -> IO Answer

--                           result
class BlockchainStateClass b r where
    updateUsingBlock :: Block -> b -> IO (b, r)
    -- inssuficient

class PureUpdateWithBlock s r where
    pureUpdate :: Block -> s -> (r, s)
    -- maybe better for r to be only an information

class PureUpdateWithBlock2 s r where
    pureUpdate :: Block -> s -> r

class AtomicModifyIO2 tvar_s r where
    atomicModifyIO2 :: tvar_s -> r -> IO ()


-- class Monad m => InMemoryRead tvar m a where
--     readMemory :: tvar -> IO a

class Monad m => InMemory tvar m a where
    runAtomically :: m b -> IO b
    readMemory :: tvar -> m a
    writeMemory :: tvar -> a -> m ()
    modifyMemory :: tvar -> (a -> a) -> m ()
    modifyMemory tvar f = readMemory tvar >>= (writeMemory tvar . f)


instance InMemory (TVar a) STM a where
    runAtomically = atomically
    readMemory = readTVar
    modifyMemory = modifyTVar'

in practice want function :: Blockchain -> BlockchainUpdated -> (HowUpdated, Blockchain)

instance PureUpdateWithBlock BlockchainState BlockchainUpdated

class HasBlockchainState m s where 
    get :: m s
    set :: s -> m () 

-- class (HasBlockchainState m s, PureUpdateWithBlock s r) =>  where
--     r -> m ()

-- class AtomicModifyIO tvar_s s where
--     atomicModifyIO :: tvar_s -> (s -> s) -> IO ()

-- this better
class AtomicModifyIO tvar_s s where
    atomicModifyIO :: tvar_s -> (s -> (s, r)) -> IO r

clas AtomicModifyIO2 tvar_s s r where
    atomicModifyIO2 :: tvar_s -> (s -> r) -> IO r

instance AtomicModifyIO tvar_s s => Has s tvar_s where


class Memory f where
    -- maybe split
    readMemory :: f a -> IO a
    writeMemory :: a -> f a -> IO ()

instance Memory TVar where
    readMemory = readTVarIO
    writeMemory = writeTVarIO

-- instance BlockchainStateClass (STM BlockchainState) where
--     updateBlockchain

class MyData a where
    modifyMyData :: (a -> a) -> IO ()
    getMyData    :: IO a

instance MyData (TVar a) where
    modifyMyData f = atomically $ modifyTVar f
    getMyData    :: IO a
-- !!!!!!!

serve :: HasLogging m => Address -> (SockAddr -> Message -> m Answer)
serve = server addr log (serverHandler _)




handleMessage :: (HasLogging m, BlockchainStateClass b r =>
                 ForkMaxDiff
              -> TargetHash
              -> (r -> m)
              -> SockAddr
              -> Message
              -> IO Answer

handleMessage :: ForkMaxDiff
              -> TargetHash
              -> AppState
              -> (String -> IO ())
              -> SockAddr
              -> Message
              -> IO Answer
handleMessage _ _ appSt log sockaddr PingMessage = defaultPingHandler

handleMessage forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) log sockaddr (BlockMessage block) = do
    
    
    forkIO . (do 
        let (s, r) = pureUpdate block s
        put s
        react r)

    -- do the validation etc concurrently not to hold a connection for long
    forkIO . join . atomically $ do
        -- Note that this atomical operation is time-consuming.
        lively   <- readLivelyBlocks blockchainState
        fixed    <- readFixedBlocks blockchainState
        future   <- readFutureBlocks blockchainState
        utxoPool <- readUTXOPool blockchainState

        -- try to link a new block to one of the recent blocks
        case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed future of

                            
    return ReceivedBlock


handleMessage _ _ appSt@AppState {blockchainState} log sockaddr (TransactionMessage tx) = do 
    -- append new transaction to queue
    -- Transactions are initially validated against utxo's from known FixedBlocks.
    -- TODO: This should be updated to validate for state at the tip of LivelyBlocks.
    join . atomically $ do
            utxoPool <- readUTXOPool blockchainState
            if validTransaction utxoPool tx then do
                appendTransaction tx appSt
                return $ log "handler: Received new transaction."
            else 
                return $ log "handler: Didn't bother with transaction."

    return ReceivedTransaction

handleMessage _ _ (AppState {blockchainState}) log sockaddr (BlockchainQuery query) = do
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

-- Answer a query with all the contacts from out contact list.  
handleMessage _ _ (AppState {peers}) log sockaddr ContactQuery = do
    log "handler: Received a contact query."
    peers <- readTVarIO peers 
    return (ContactQueryAnswer $ getAddresses peers)


react :: BlockchainUpdated -> IO ()
react (BlockInserted fixed' lively' utxoPool') = do
    log "handler: Received block was inserted into chain."
    -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
    broadcastAndUpdatePeers peers (BlockMessage block) ReceivedBlock

react (FutureBlock future') = do 
    log "handler: Received block inserted into futures waiting list."
    -- We received a block that doesn't link to a known recent chain. Let's query for new blocks.
    catchUpToBlockchain forkMaxDiff targetHash appSt log

react BlockAlreadyInserted = log "handler: Received block was already present in the chain."

react BlockInvalid         = log "handler: Received block is invalid."

react BLockInsertedLinksToRoot lively' = do
        log "handler: Inserted block linking to genesis."
        -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
        broadcastAndUpdatePeers peers (BlockMessage block) ReceivedBlock








{-# LANGUAGE RecordWildCards, DeriveGeneric, NamedFieldPuns, LambdaCase, ScopedTypeVariables #-}
module App where

newtype PeersSet = PeersSet (Map.Map Address Status)
    deriving (Generic)

broadcastWithReply_ :: TVar PeersSet -> Message -> Answer -> (Maybe Answer -> IO ()) -> IO ()

updatePeers :: HasPeerSet env => RIO env ()
    
data BlockchainState = BlockchainState Genesis (TVar FixedBlocks) (TVar LivelyBlocks) (TVar FutureBlocks) (TVar UTXOPool)

readFutureBlocks :: BlockchainState -> STM FutureBlocks
...
writeUTXOPool :: BlockchainState -> UTXOPool -> STM ()

data AppState bs = AppState {
    blockchainState :: bs,   
    incomingTxs  :: TVar (Seq.Seq Transaction),
    peers        :: TVar PeersSet,
    minerWallet  :: TVar SimpleWallet   -- ?
    }

data LoggingMode = ToFile FilePath | ToStdin | Silent deriving Generic

data Config = Config {
        blockchainFilepath :: FilePath,
        peersFilepath :: FilePath,
        targetDifficulty   :: Int,
        loggingMode :: LoggingMode,
        port        :: ServiceName,
        blockchainGenesis     :: Genesis,
        minerWaitForTxs :: Bool
    } deriving (Generic)

-- not sure which interface am i going to prefer - cmdline options or config file 
data CommandOptions = CommandOptions {
    configFilepath :: FilePath
}

parseCommand :: ParserInfo CommandOptions

createBlockchainState' :: Genesis -> Block -> STM BlockchainState

-- TODO: Add to config.
forkMaxDiff :: ForkMaxDiff

-- To be used after broadcasting message to update PeersSet based on activity of the addressee's. 
expectAnswer HasPeerSet env => -- collection of addresses and their activity status 
             -> Address        -- addressee 
             -> Answer         -- the answer we expect
             -> Maybe Answer   -- the answer we got
             -> RIO env ()          -- updating of PeersSet according to address activity status

handleMessage :: (HasForkMaxDiff env, HasTargetHash env, HasAppState env, HasLogging env) =>
              -> SockAddr
              -> Message
              -> RIO env Answer
handleMessage _ _ appSt log sockaddr PingMessage = do
handleMessage forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) log sockaddr (BlockMessage block) = do
handleMessage _ _ appSt@AppState {blockchainState} log sockaddr (TransactionMessage tx) = do
handleMessage _ _ (AppState {blockchainState}) log sockaddr (BlockchainQuery query) = do
handleMessage _ _ (AppState {peers}) log sockaddr ContactQuery = do


catchUpToBlockchain :: ForkMaxDiff
                    -> TargetHash
                    -> AppState
                    -> (String -> IO ())
                    -> IO ()

-- Constant
keyLength :: Int

mining :: TargetHash        -- Need hash ∈ [0, targetHash] 
       -> AppState
       -> Bool              -- Do we wait for transaction or produce coinbase-only blocks
       -> (String -> IO ()) -- Logging function
       -> IO ()

-- Returns when transaction appears in blockchain.
broadcastTransaction :: AppState -> Transaction -> IO ()

makeTransaction :: AppState -> PublicAddress -> Cent -> IO (Maybe Transaction)
        
serverHandler :: (SockAddr -> Message -> RIO env Answer) -> (String -> IO ()) -> HandlerFunc

fullNode :: (HasLogging env, HasEverything env) => RIO env ()
fullNode = runC

fullNode = runApp initFullState (mine ++ )

-- runApp :: RIO env a -> = 
--     bracket $ 
--         init all state
--         runConcurently (server handler) ...
--         quit and save

run :: (Config -> IO a) -> IO a  = 
    bracket
        read argument filepath and open config
        runApp config (maybe runReader runApp config or sth)
        close config

-- AppState, running main (mining, server), logging function
newtype RunningApp = RunningApp (AppState, Async (), String -> IO ())
-- bad idea?

runNode :: Config -> IO (Maybe RunningApp)

-- Launch app to do stuff and quit
withAppDoAndQuit :: Config -> (AppState -> (String -> IO ()) -> IO a) -> IO (Maybe a)

-- Launch app, do stuff and live the app running.
withAppDo :: Config -> (AppState -> (String -> IO ()) -> IO ()) -> IO ()


loging :: MonadIO m => String -> m ()
loging str = liftIO (print str)

main = loging "hello" :: IO ()