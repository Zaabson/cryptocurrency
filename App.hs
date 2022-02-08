{-# LANGUAGE RecordWildCards, DeriveGeneric, NamedFieldPuns, LambdaCase, ScopedTypeVariables #-}
module App where

import Server (Address(..), server, ServerHandler)
import BlockType (Block (blockHeader), Transaction, BlockHeader (timestamp), blockBlockHeight, Genesis (Genesis), blockPreviousHash, BlockReference, Cent (Cent), PublicAddress)
import System.IO (withBinaryFile, IOMode (ReadMode, AppendMode), hPutStr, withFile, Handle, hPutStrLn)
import Data.Aeson (encode, decode, encodeFile, FromJSON, ToJSON, decodeFileStrict, eitherDecodeFileStrict)
import qualified Data.ByteString.Lazy as LazyB (hGetContents)
import Control.Concurrent.STM (TVar, STM, modifyTVar', stateTVar, newTVarIO, readTVarIO, newTVar, readTVar, atomically, writeTVar, modifyTVar, retry, check, orElse)
import BlockChain (LivelyBlocks (LivelyBlocks), FixedBlocks (FixedBlocks, getFixedBlocks), Blockchain(..), insertToChain, newTree, ForkMaxDiff(ForkMaxDiff), updateWithBlock, BlockchainUpdated (..), collectUTXOs, getLastBlock, FutureBlocks (FutureBlocks), root, forest)
import qualified Data.Sequence as Seq (Seq, (|>), take, length, splitAt, empty)
import Control.Arrow (first, Arrow ((&&&), (***)))
import GHC.Generics (Generic)
import Options.Applicative
import Data.Maybe (fromJust, fromMaybe)
import Control.Concurrent (MVar, forkIO, withMVar, newMVar, threadDelay)
import Control.Monad (void, forever, liftM2, when, join, liftM3)
import Control.DeepSeq
import Data.Time (getCurrentTime, hoursToTimeZone, formatTime, defaultTimeLocale, getZonedTime, UTCTime, ZonedTime, NominalDiffTime, diffUTCTime, zonedTimeToUTC)
import Network.Socket (ServiceName, SockAddr, Family (AF_802))
import MessageType (Message (..), Answer(..), BlockchainQuery (BlockAtHeight), BlockchainQueryResult (NoBlockFound, RequestedBlock))
import Control.Exception (onException, evaluate)
import Hashing (TargetHash(TargetHash), RawHash(RawHash), shash256)
import qualified Data.ByteString as B
import Control.Monad.Reader (ReaderT)
import BlockValidation (UTXOPool, UTXO (UTXO), validateBlock, validTransaction)
import qualified Data.Map as Map
import BlockCreation (Keys(Keys), OwnedUTXO (OwnedUTXO), mineBlock, blockRef, SimpleWallet, createSendingTransaction)
import qualified Codec.Crypto.RSA as RSA
import Crypto.Random.DRBG (newGenIO)
import Data.Foldable (toList, find, Foldable (foldl'))
import Control.Concurrent.Async (async, race, waitEitherCancel, waitSTM, race_, forConcurrently_, forConcurrently, concurrently_, Async, waitBoth)
import qualified Crypto.Random.DRBG as DRBG
import Client (sendToAll, send, sendAndReceive)
import Numeric.Sampling (sampleIO)
import Control.Monad.STM (check)
import GHC.Conc.Sync (readTVarIO)
import Data.Time.Clock (getCurrentTime)
import Data.Aeson.Types (FromJSON)
import GHC.Conc (readTVarIO)
import Data.Word (Word8)

-- data MiningMode
--     = Mining
--     | Idle

-- loadData == decodeFileStrict !!!
-- loadData :: FromJSON a => FilePath -> IO (Maybe a)
-- loadData path = withBinaryFile path ReadMode ((decode <$>) . LazyB.hGetContents) 

sendMsg :: Message -> Address -> IO ()
sendMsg msg = send (encode msg)

sendMsgToAll :: Message -> [Address] -> IO ()
sendMsgToAll msg = sendToAll (encode msg) 

sendAndReceiveMsg ::  Message -> (Maybe Answer -> IO a) -> Address -> IO a
sendAndReceiveMsg msg k address = sendAndReceive (encode msg) address (k . (>>=  decode))

-- saveBlockChain == Data.Aeson.encodeFile
-- loadBlockchain == Data.Aeson.decodeFileStrict

-- data Save = Save LivelyBlocks (Seq.Seq Transaction)
    -- deriving (Show, Generic)

-- instance ToJSON Save 
-- instance FromJSON Save

-- makeAppState :: PeersList -> Seq.Seq Transaction -> FixedBlocks -> LivelyBlocks -> STM AppState
-- makeAppState peers icomingTxs fixed lively = do
--     recentBlocks <- newTVar lively
--     oldBlocks     <- newTVar fixed
--     incomingTxs   <- newTVar txs
--     peers         <- newTVar peers
--     return $ AppState 

-- probably will be used for manual testing
--             Fixedblocks path, Save path
-- loadSavedState :: FilePath -> FilePath -> FilePath -> IO (Maybe AppState)
-- loadSavedState fixedpath savepath networkpath = do 
--     mfixed <- loadData fixedpath
--     msave <- loadData savepath
--     mnetwork <- loadData networkpath
--     case (mfixed, msave, mnetwork) of
--         (Just fixed, Just (Save lively txs), Just peers) -> do
--             recentBlocks <- newTVarIO lively
--             oldBlocks    <- newTVarIO fixed
--             incomingTxs  <- newTVarIO txs
--             peers'        <- newTVarIO peers
--             return $ Just $ AppState {recentBlocks = recentBlocks, 
--                                       oldBlocks    = oldBlocks,
--                                       incomingTxs  = incomingTxs,
--                                       peers        = peers' }
--         _ -> return Nothing

-- saveAppState :: FilePath -> FilePath -> AppState -> IO ()
-- saveAppState fixedpath savepath AppState {..} = do 
--     lively <- readTVarIO recentBlocks
--     fixed <- readTVarIO oldBlocks
--     txs <- readTVarIO incomingTxs
--     encodeFile fixedpath fixed
--     encodeFile savepath $ Save lively txs

-- blockchainToAppState :: Blockchain -> STM AppState
-- blockchainToAppState (Blockchain fixed lively genesis) = do 
--     tlively <- newTVar lively
--     toldBlocks <- newTVar fixed
--     tincomingTxs <- newTVar Seq.empty
--     return $ AppState tlively toldBlocks tincomingTxs

data Status
    = Active
    | DidntReact UTCTime
    deriving (Generic)

instance FromJSON Status 
instance ToJSON Status

-- newtype Peer = Peer Address Status

newtype PeersSet = PeersSet (Map.Map Address Status)
    deriving (Generic)

instance FromJSON PeersSet 
instance ToJSON PeersSet

getAddresses :: PeersSet -> [Address]
getAddresses (PeersSet peers) = Map.keys peers 

getPeers :: PeersSet -> [(Address, Status)]
getPeers (PeersSet peers) = Map.assocs peers

-- Broadcast message to peers and update their statuses based on matching the expected answer.
broadcast :: TVar PeersSet -> Message -> Answer -> IO ()
broadcast peers msg answer = do
    peersSet <- readTVarIO peers
    forConcurrently_ (getAddresses peersSet) $ \addr ->
        sendAndReceiveMsg msg (expectAnswer peers addr answer) addr

-- Broadcast message to peers and update their statuses based on matching the expected answer.
-- This can delete some contacts so we run updatePeers afterwards to keep the number of contacts high.
broadcastAndUpdatePeers :: TVar PeersSet -> Message -> Answer -> IO ()
broadcastAndUpdatePeers peers msg answer = do
    broadcast peers msg answer
    updatePeers peers

-- Broadcast message to peers and update their statuses based on matching the expected answer.
-- Also react to their answers with provided function.
broadcastWithReply :: TVar PeersSet -> Message -> Answer -> (Maybe Answer -> IO a) -> IO [a]
broadcastWithReply peers msg answer f = do
    peersSet <- readTVarIO peers
    forConcurrently (getAddresses peersSet) $ \addr ->
        sendAndReceiveMsg
            msg 
            (\manswer -> do 
                expectAnswer peers addr answer manswer
                f manswer)
            addr

broadcastWithReply_ :: TVar PeersSet -> Message -> Answer -> (Maybe Answer -> IO ()) -> IO ()
broadcastWithReply_ peers msg answer f = void $ broadcastWithReply peers msg answer f



-- If there's less than 10 addresses in our PeersSet ask all peers for their peers address lists.
-- Ping received addresses and add to PeersSet ones that responded.
updatePeers :: TVar PeersSet -> IO ()
updatePeers peers = do 
    PeersSet peersSet <- readTVarIO peers
    when (Map.size peersSet < 10) $
        broadcastWithReply_ peers ContactQuery (ContactQueryAnswer []) (\case 
                Just (ContactQueryAnswer xs) -> pingAndAdd peers xs
                _ -> return ())

    where 
        -- Ping addresses and add to PeersSet all that responded.
        pingAndAdd :: TVar PeersSet -> [Address] -> IO ()
        pingAndAdd peers addresses =
            forConcurrently_ addresses (\addr -> 
                sendAndReceiveMsg
                    PingMessage
                    (\case 
                        Just PingAnswer -> atomically $ modifyTVar' peers (\(PeersSet map) -> PeersSet $ Map.insert addr Active map)
                        _ -> return ())
                    addr)

-- newtype PeersList = PeersList [Address]
--     deriving (Show, Generic)

-- instance ToJSON PeersList
-- instance FromJSON PeersList

-- updateStatus :: 


-- LivelyBlocks by definition contains at least one block, so we differentiate between the empty case.
-- The invariant is that root of LivelyBlocks is the next block after the head of FixedBlocks.
-- Also UTXOPool is a pool of txs up of all FixedBlocks.
-- type BlockchainState = Either Genesis BlockchainState
    
data BlockchainState = BlockchainState Genesis (TVar FixedBlocks) (TVar LivelyBlocks) (TVar FutureBlocks) (TVar UTXOPool)

readFixedBlocks :: BlockchainState -> STM FixedBlocks
readFixedBlocks (BlockchainState _ fixed _ _ _) = readTVar fixed

readLivelyBlocks :: BlockchainState -> STM LivelyBlocks
readLivelyBlocks (BlockchainState _ _ lively _ _) = readTVar lively

readFutureBlocks :: BlockchainState -> STM FutureBlocks
readFutureBlocks (BlockchainState _ _ _ future _) = readTVar future

readUTXOPool :: BlockchainState -> STM UTXOPool
readUTXOPool (BlockchainState _ _ _ _ utxoPool) = readTVar utxoPool

writeFixedBlocks :: BlockchainState -> FixedBlocks -> STM ()
writeFixedBlocks (BlockchainState _ fixed _ _ _) = writeTVar fixed

writeLivelyBlocks :: BlockchainState -> LivelyBlocks -> STM ()
writeLivelyBlocks (BlockchainState _ _ lively _ _) = writeTVar lively

writeFutureBlocks :: BlockchainState -> FutureBlocks -> STM ()
writeFutureBlocks (BlockchainState _ _ _ future _) = writeTVar future

writeUTXOPool :: BlockchainState -> UTXOPool -> STM ()
writeUTXOPool (BlockchainState _ _ _ _ utxoPool) = writeTVar utxoPool



-- probably unneeded? Genesis is only important for empty blockchain 
-- getGenesis :: BlockchainState -> Genesis
-- getGenesis BlockchainState _ _ _ _ genesis = genesis

-- TODO: Investigate all the places where I forgot about genesis.

data AppState = AppState {
    blockchainState :: BlockchainState,   
    incomingTxs  :: TVar (Seq.Seq Transaction),
    peers        :: TVar PeersSet,
    minerWallet  :: TVar SimpleWallet 
    }

appendTransaction :: Transaction -> AppState -> STM ()
appendTransaction txs AppState {..} = modifyTVar' incomingTxs (Seq.|> txs)

-- -- Take n transaction or fail if there is less
-- takeNTransactions :: Int -> AppState -> STM (Maybe (Seq.Seq Transaction))
-- takeNTransactions n AppState {..} = stateTVar incomingTxs $ \txs ->
--     if Seq.length txs >= n then
--         first Just $ Seq.splitAt n txs
--     else
--         (Nothing, txs)

data LoggingMode = ToFile FilePath | ToStdin | Silent deriving Generic
instance ToJSON LoggingMode
instance FromJSON LoggingMode


data Config = Config {
        blockchainFilepath :: FilePath,
        peersFilepath :: FilePath,
        targetDifficulty   :: Int,
        loggingMode :: LoggingMode,
        port        :: ServiceName,
        blockchainGenesis     :: Genesis,
        minerWaitForTxs :: Bool
    } deriving (Generic)

instance ToJSON Config
instance FromJSON Config

-- not sure which interface am i going to prefer - cmdline options or config file 
data CommandOptions = CommandOptions {
    configFilepath :: FilePath
}

parseCommand :: ParserInfo CommandOptions
parseCommand = info parseCommand
    (fullDesc <> progDesc "Run a full node.")
    where 
        parseCommand = CommandOptions
            <$> strOption
                (  long "config"
                <> metavar "TARGET"
                <> help "Filepath for the config file" )

-- takes block and creates the BlockchainState for a single block assuming that it links to Genesis
createBlockchainState' :: Genesis -> Block -> STM BlockchainState
createBlockchainState' genesis block =
    BlockchainState genesis <$> newTVar (FixedBlocks [])
                             <*> newTVar (LivelyBlocks {root=shash256 (Left genesis), forest=[newTree block]})
                             <*> newTVar (FutureBlocks Map.empty)
                             <*> newTVar Map.empty

-- TODO: Add to config.
forkMaxDiff :: ForkMaxDiff
forkMaxDiff = ForkMaxDiff 2

-- diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
-- diffZonedTime time1 time2 = diffUTCTime (zonedTimeToUTC time1) (zonedTimeToUTC time2)

-- To be used after broadcasting message to update PeersSet based on activity of the addressee's. 
expectAnswer :: TVar PeersSet  -- collection of addresses and their activity status 
             -> Address        -- addressee 
             -> Answer         -- the answer we expect
             -> Maybe Answer   -- the answer we got
             -> IO ()          -- updating of PeersSet according to address activity status
expectAnswer peers address expected answer = do
    time <- getCurrentTime
    atomically $ modifyTVar' peers (\(PeersSet peers) -> 
        PeersSet $ Map.alter 
            (case answer of
                Nothing -> didntAnswer time
                Just answer | not (answerEqConstr expected answer) -> didntAnswer time
                Just answer -> didAnswer)
            address peers)
    where
        didntAnswer :: UTCTime -> Maybe Status -> Maybe Status 
        didntAnswer time (Just Active)                = Just (DidntReact time)
        didntAnswer time (Just (DidntReact lasttime)) =
            if diffUTCTime time lasttime > 24 * 60 * 60 then Nothing else Just (DidntReact time)
        didntAnswer time Nothing                    = Nothing

        didAnswer :: Maybe Status -> Maybe Status
        didAnswer = const (Just Active)


handleMessage :: ForkMaxDiff
              -> TargetHash
              -> AppState
              -> (String -> IO ())
              -> SockAddr
              -> Message
              -> IO Answer
handleMessage _ _ appSt log sockaddr PingMessage = do
    log "handler: Received Ping."
    return PingAnswer

handleMessage forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) log sockaddr (BlockMessage block) = do
    
    -- do the validation etc concurrently not to hold a connection for long
    forkIO . join . atomically $ do
        -- Note that this atomical operation is time-consuming.
        lively   <- readLivelyBlocks blockchainState
        fixed    <- readFixedBlocks blockchainState
        future   <- readFutureBlocks blockchainState
        utxoPool <- readUTXOPool blockchainState

        -- try to link a new block to one of the recent blocks
        case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed future of
            BlockInserted fixed' lively' utxoPool' -> do
                writeLivelyBlocks blockchainState lively'
                writeFixedBlocks blockchainState fixed'
                writeUTXOPool blockchainState utxoPool'
                return $ do
                    log "handler: Received block was inserted into chain."
                    -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                    broadcastAndUpdatePeers peers (BlockMessage block) ReceivedBlock
            FutureBlock future'   -> do
                writeFutureBlocks blockchainState future'
                return $ do 
                    log "handler: Received block inserted into futures waiting list."
                    -- We received a block that doesn't link to a known recent chain. Let's query for new blocks.
                    catchUpToBlockchain forkMaxDiff targetHash appSt log
            BlockAlreadyInserted -> return $ log "handler: Received block was already present in the chain."
            BlockInvalid         -> return $ log "handler: Received block is invalid."
            BLockInsertedLinksToRoot lively' -> do
                writeLivelyBlocks blockchainState lively'
                return $ do
                    log "handler: Inserted block linking to genesis."
                    -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                    broadcastAndUpdatePeers peers (BlockMessage block) ReceivedBlock
                            
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

catchUpToBlockchain :: ForkMaxDiff
                    -> TargetHash
                    -> AppState
                    -> (String -> IO ())
                    -> IO ()
catchUpToBlockchain forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) log = do
    blocks <- queryForBlocks
    atomically $ do
        fixed <- readFixedBlocks blockchainState
        utxoPool <- readUTXOPool blockchainState
        lively <- readLivelyBlocks blockchainState
        future <- readFutureBlocks blockchainState
        let (utxoPool', fixed', lively', future') = foldl' update (utxoPool, fixed, lively, future) blocks
        writeLivelyBlocks blockchainState lively'
        writeFutureBlocks blockchainState future'
        writeFixedBlocks blockchainState fixed'
        writeUTXOPool blockchainState utxoPool' 


    where
        -- TODO: refactor with broadcastWithReply 
        -- queries given address for blocks starting at height n and querying as long as we get valid answers.
        keepQuerying :: Integer -> Address -> IO [Block]
        keepQuerying n address = do
            sendAndReceiveMsg (BlockchainQuery $ BlockAtHeight n) (\manswer -> do
                expectAnswer peers address (BlockchainQueryAnswer NoBlockFound) manswer
                case manswer of 
                    Just (BlockchainQueryAnswer (RequestedBlock b))  -> (b :) <$> keepQuerying (n+1) address
                    _ -> return [] ) address
        
        queryForBlocks :: IO [Block]
        queryForBlocks = do
            -- get the length of the blockchain we have, we ask for the next block
            n <- atomically $ do
                FixedBlocks fixed <-readFixedBlocks blockchainState
                return $ 1 + case fixed of
                                []  -> 0
                                b:_ -> blockBlockHeight b

            -- get random selection of 1 to 8 addresses
            addresses <- getAddresses <$> readTVarIO peers
            maddresses <- sampleIO (max 1 (min 8 (length addresses))) addresses

            case maddresses of
                Nothing -> do
                    log "Failed to query for new blocks. Not enough peers."
                    return []
                Just addresses -> do 
                    log "Queried for new blocks."
                    concat <$> forConcurrently addresses (keepQuerying n)
            

        update :: (UTXOPool, FixedBlocks, LivelyBlocks, FutureBlocks) -> Block -> (UTXOPool, FixedBlocks, LivelyBlocks, FutureBlocks)
        update (utxoPool, fixed, lively, future) block = 
            -- try to link a new block to one of the recent blocks
            case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed future  of
                BlockInserted fixed' lively' utxoPool' -> (utxoPool', fixed', lively', future)
                BLockInsertedLinksToRoot lively'       -> (utxoPool, fixed, lively', future)
                FutureBlock future'   -> (utxoPool, fixed, lively, future')
                _                     -> (utxoPool, fixed, lively, future)

-- Constant
keyLength :: Int
keyLength = 2048

generateKeys :: IO Keys
generateKeys = do
    g <- newGenIO :: IO DRBG.HmacDRBG
    let (pub, priv, _) = RSA.generateKeyPair g keyLength
    return $ Keys pub priv

blocksEqual :: Block -> Block -> Bool
blocksEqual b1 b2 = shash256 (blockHeader b1) == shash256 (blockHeader b2)


-- This needs some more thinking. After mining a block few things should be done atomic? maybe not needed 
-- Also combining those waits will be tricky 

mining :: TargetHash        -- Need hash ∈ [0, targetHash] 
       -> AppState
       -> Bool              -- Do we wait for transaction or produce coinbase-only blocks
       -> (String -> IO ()) -- Logging function
       -> IO ()
mining targetHash (AppState {blockchainState, incomingTxs, peers}) waitForTxs log = forever $ do
    
    (lastblockRef, height)  <- atomically getLastBlockReference

    -- find pending Transaction's to include in the Block
    txs <- 
        if waitForTxs then
            atomically $ do
                txs <- readTVar incomingTxs
                if null txs then
                    retry
                else
                    return txs
        else
            readTVarIO incomingTxs

    -- TODO: waitingForNewLastBlock returns too often, suss
    -- doMining lastblockRef height (toList txs) `race_` threadDelay 240000000 `race_` atomically (waitingForNewLastBlock lastblockRef)
    doMining lastblockRef height (toList txs) `race_` threadDelay 240000000

    where

        -- Calculates the reference to the furthest block in the blockchain - that is furthest leaf from LivelyBlocks 
        -- or LivelyBlocks root if LivelyBlocks is empty (then it is Genesis reference)
        getLastBlockReference :: STM (BlockReference, Integer)
        getLastBlockReference = do
            lively@LivelyBlocks {root, forest} <- readLivelyBlocks blockchainState
            FixedBlocks fixed <- readFixedBlocks blockchainState
            case getLastBlock lively of
                -- LivelyBlocks is empty.
                Nothing -> return (root, 
                    case fixed of 
                        [] -> 1
                        b:bs -> 1 + blockBlockHeight b)
                Just lastblock -> return (blockRef lastblock, 1 + blockBlockHeight lastblock)

        waitingForNewLastBlock :: BlockReference -> STM ()
        waitingForNewLastBlock oldRef = do
            (ref, _) <- getLastBlockReference
            check (ref == oldRef)

        doMining :: BlockReference -> Integer -> [Transaction] -> IO ()
        doMining lastblockRef height txs = do
            timestamp <- getCurrentTime
            -- keys for coinbase money
            keys <- generateKeys

            -- mine a block
            let (ownedUTXO, block) = mineBlock targetHash keys timestamp txs height lastblockRef

            -- log "Start mining."

            -- forces hash crunching
            evaluate . force $ blockHeader block 

            -- collect utxo in wallet:
            -- TODO
            log "We mined a coin!"
            -- _ ownedUTXO

            -- Put the newly mined block into blockchain.
            -- Broadcast the block to others.
            join . atomically $ do
                -- Note that this atomical operation is time-consuming.
                lively   <- readLivelyBlocks blockchainState
                fixed    <- readFixedBlocks blockchainState
                future   <- readFutureBlocks blockchainState
                utxoPool <- readUTXOPool blockchainState

                case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed future of
                    BlockInserted fixed' lively' utxoPool' -> do
                        writeLivelyBlocks blockchainState lively'
                        writeFixedBlocks blockchainState fixed'
                        writeUTXOPool blockchainState utxoPool'
                        return $
                            -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                            broadcastAndUpdatePeers peers (BlockMessage block) ReceivedBlock
                    BLockInsertedLinksToRoot lively' -> do
                        writeLivelyBlocks blockchainState lively'
                        return $
                            -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                            broadcastAndUpdatePeers peers (BlockMessage block) ReceivedBlock
                    _ -> return $ log "Warning! Mined block is fucked up."

-- Usefull????

-- Returns when transaction appears in blockchain.
broadcastTransaction :: AppState -> Transaction -> IO ()
broadcastTransaction (AppState {peers, blockchainState}) tx = do
    -- -- we dont call updatePeers here for it to run fastest --
    broadcastAndUpdatePeers peers (TransactionMessage tx) ReceivedTransaction
    let txid = shash256 (Right tx)
    -- keep checking whether at least first output from transaction appears in UTXOPool
    atomically $ do
        utxoPool <- readUTXOPool blockchainState
        check (Map.member (txid, 0) utxoPool)

makeTransaction :: AppState -> PublicAddress -> Cent -> IO (Maybe Transaction)
makeTransaction (AppState {minerWallet}) recipient amount = do
    keys <- generateKeys
    atomically $ do
        wallet <- readTVar minerWallet
        case createSendingTransaction wallet keys recipient amount of 
            Nothing -> return Nothing  
            Just (wallet', tx) -> do
                writeTVar minerWallet wallet'
                return (Just tx)

-------------------
        
-- serverHandler :: (SockAddr -> Message -> IO Answer) -> (String -> IO ()) -> HandlerFunc
-- serverHandler handler log sockAddr msgbytes = do
--     case decode msgbytes of 
--         Nothing -> do 
--             log "handler: Unable to parse message."
--             return $ encode MessageParseError
--         Just msg -> do
--             answer <-  handler sockAddr msg
--             return $ encode answer


-- TODO: Who creates genesis block? is it part of running a node? 
-- IRL genesis and some initial contact list would be settled on outside the protocol or hardcoded into the protocol.

-- Recalculate UTXOPool on start or load it from file?
-- TODO: Load it ^ from file.

-- AppState, running main (mining, server), logging function
newtype RunningApp = RunningApp (AppState, Async (), String -> IO ())

runNode :: Config -> IO (Maybe RunningApp)
runNode config = do
    -- make logging function
    loggerlock <- newMVar ()
    let log = logger (loggingMode config) loggerlock
    
    let targetHash = difficultyToTargetHash $ targetDifficulty config

    -- TODO: optional cmd arg to load state from save, otherwise only loads

    -- mpeers      <- loadData (peersFilepath config)      `onException` log "app: Failed to load peers from file. Exits."
    -- mblockchain <- loadData (blockchainFilepath config) `onException` log "app: Failed to load blockchain from file. Exits."
    eitherPeers <- eitherDecodeFileStrict (peersFilepath config)
    eitherBlockchain <- eitherDecodeFileStrict (blockchainFilepath config)

    case eitherPeers of
        Left err    -> do
            -- TODO: Idea: change return type to Either and let user handle different errors
            log err
            log "app: Couldn't open or parse peers file. Quits."
            -- TODO: Bug: App quits before logging errors.
            return Nothing
        Right peers ->
            case eitherBlockchain of
                -- Nothing -> log "app: Couldn't parse blockchain file."
                Left err -> do
                    log err
                    log "app: Failed to load blockchain from file. Quits."
                    return Nothing
                Right fixed -> do
                    blockchainState <- BlockchainState (blockchainGenesis config)
                            <$> newTVarIO fixed
                            <*> newTVarIO (LivelyBlocks {
                                    root=case fixed of 
                                        FixedBlocks [] -> shash256 (Left $ blockchainGenesis config)
                                        FixedBlocks (b:bs) -> blockRef b, forest=[]})
                            <*> newTVarIO (FutureBlocks Map.empty)
                            <*> newTVarIO (collectUTXOs Map.empty (getFixedBlocks fixed))

                    log "app: Loaded fixed blocks."
                    
                    appState <-
                        AppState blockchainState
                            <$> newTVarIO Seq.empty
                            <*> newTVarIO peers
                            <*> newTVarIO []
    
                    -- TODO: catching up to  blockchain
                    -- query for blocks after our last block
                    forkIO $ catchUpToBlockchain forkMaxDiff targetHash appState log

                    let mine = mining targetHash appState (minerWaitForTxs config) log 

                    let serverAddr = Address "localhost" (port config)
                    let runServer = server serverAddr log (serverHandler (handleMessage forkMaxDiff targetHash appState log) log)
            
                    -- forkIO runServer
                    -- forkIO mine
                    main <- async $ concurrently_ mine runServer

                    return (Just $ RunningApp (appState, main, log))

    where            

        -- how to open only a single handle but also have it closed automaticaly?
        --                             name to log under, message to log
        logger :: LoggingMode -> MVar () -> String -> IO ()
        logger Silent _ _ = return ()
        logger mode lock str       = void $ forkIO $ withMVar lock (\a -> do
            time <- getZonedTime
            -- TODO: fix time not displaying properly
            let timeinfo = formatTime defaultTimeLocale "%T" time
            let msg =  timeinfo ++ ": " ++ str
            case mode of 
                ToStdin     -> putStrLn msg
                ToFile path -> withFile path AppendMode (`hPutStrLn` msg)
            return a)


-- log :: RunningApp -> String ->  IO ()
-- log (RunningApp (appSt, _, log)) = log 

-- Launch app to do stuff and quit
withAppDoAndQuit :: Config -> (AppState -> (String -> IO ()) -> IO a) -> IO (Maybe a)
withAppDoAndQuit = undefined

-- Launch app, do stuff and live the app running.
withAppDo :: Config -> (AppState -> (String -> IO ()) -> IO ()) -> IO ()
withAppDo config action = runNode config >>= \case
    Nothing -> -- error is logged in runnode already
        return ()
    Just (RunningApp (appSt, main, log)) -> do
        action <- async $ action appSt log 
        void $ waitBoth action main
