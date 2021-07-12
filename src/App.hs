{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App where

import Server (Address(..), server, HandlerFunc)
import BlockType (Block (blockHeader), Transaction, BlockHeader (timestamp), blockBlockHeight)
import System.IO (withBinaryFile, IOMode (ReadMode, AppendMode), hPutStr, withFile, Handle)
import Data.Aeson (encode, decode, encodeFile, FromJSON, ToJSON, decodeFileStrict)
import qualified Data.ByteString.Lazy as LazyB (hGetContents)
import Control.Concurrent.STM (TVar, STM, modifyTVar', stateTVar, newTVarIO, readTVarIO, newTVar, readTVar, atomically, writeTVar, modifyTVar, retry, check, orElse)
import BlockChain (LivelyBlocks (LivelyBlocks), FixedBlocks (FixedBlocks), Blockchain(..), insertToChain, newTree, ForkMaxDiff(ForkMaxDiff), updateWithBlock, BlockchainUpdated (..), collectUTXOs, getLastBlock, FutureBlocks (FutureBlocks))
import qualified Data.Sequence as Seq (Seq, (|>), take, length, splitAt, empty)
import Control.Arrow (first, Arrow ((&&&), (***)))
import GHC.Generics (Generic)
import Options.Applicative
import Data.Maybe (fromJust)
import Control.Concurrent (MVar, forkIO, withMVar, newMVar, threadDelay)
import Control.Monad (void, forever, liftM2, when)
import Data.Time (getCurrentTime, hoursToTimeZone, formatTime, defaultTimeLocale)
import Network.Socket (ServiceName, SockAddr, Family (AF_802))
import Message (Message (..), Answer(..), Query (BlockAtHeight), QueryResult (NoBlockFound, RequestedBlock))
import Control.Exception (onException)
import Hashing (TargetHash(TargetHash), RawHash(RawHash), shash256)
import qualified Data.ByteString as B
import Control.Monad.Reader (ReaderT)
import BlockValidation (UTXOPool, UTXO (UTXO))
import qualified Data.Map as Map
import BlockCreation (Keys(Keys), mineAfterBlock, OwnedUTXO (OwnedUTXO))
import qualified Codec.Crypto.RSA as RSA
import Crypto.Random.DRBG (newGenIO)
import Data.Foldable (toList, find, Foldable (foldl'))
import Control.Concurrent.Async (async, race, waitEitherCancel, waitSTM, race_, forConcurrently_, forConcurrently)
import qualified Crypto.Random.DRBG as DRBG
import Client (sendToAll, send, sendAndReceive)
import Numeric.Sampling (sampleIO)

-- data MiningMode
--     = Mining
--     | Idle

loadData :: FromJSON a => FilePath -> IO (Maybe a)
loadData path = withBinaryFile path ReadMode ((decode <$>) . LazyB.hGetContents) 

sendMsg :: Message -> Address -> IO ()
sendMsg msg = send (encode msg)

sendMsgToAll :: Message -> [Address] -> IO ()
sendMsgToAll msg = sendToAll (encode msg) 

sendAndReceiveMsg :: Message -> Address -> (Maybe Answer -> IO a) -> IO a
sendAndReceiveMsg msg address k = sendAndReceive (encode msg) address (k . (>>=  decode))

-- saveBlockChain == Data.Aeson.encodeFile
-- loadBlockchain == Data.Aeson.decodeFileStrict

data Save = Save LivelyBlocks (Seq.Seq Transaction)
    deriving (Show, Generic)

instance ToJSON Save 
instance FromJSON Save

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

newtype PeersList = PeersList [Address]
    deriving (Show, Generic)

instance ToJSON PeersList
instance FromJSON PeersList

data AppState = AppState {
    recentBlocks :: TVar LivelyBlocks,
    oldBlocks    :: TVar FixedBlocks,
    futureBlocks :: TVar FutureBlocks,
    incomingTxs  :: TVar (Seq.Seq Transaction),
    peers        :: TVar PeersList,
    utxoPool     :: TVar UTXOPool 
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

data LoggingMode = ToFile FilePath | ToStdin deriving Generic
instance ToJSON LoggingMode
instance FromJSON LoggingMode


data Config = Config {
        blockchainFilepath :: FilePath,
        peersFilepath :: FilePath,
        targetDifficulty   :: Int,
        loggingMode :: LoggingMode,
        port        :: ServiceName
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

-- TODO: Add to config.
forkMaxDiff :: ForkMaxDiff
forkMaxDiff = ForkMaxDiff 2  

handleMessage :: ForkMaxDiff
              -> TargetHash
              -> AppState
              -> (String -> IO ())
              -> SockAddr
              -> Message
              -> IO Answer
handleMessage _ _ appSt log sockaddr PingMessage = do
    log "handler: Received Ping."
    return AnswerPing

handleMessage forkMaxDiff targetHash (AppState {recentBlocks, oldBlocks, futureBlocks, utxoPool, peers}) log sockaddr (BlockMessage block) = do
    
    -- do the validation etc concurrently not to hold a connection for long
    forkIO $ do
        (logmsg :: String, broadcastFurther :: Bool) <- atomically $ do 
            -- Note that this atomical operation is time-consuming.

            lively   <- readTVar recentBlocks
            fixed    <- readTVar oldBlocks
            future   <- readTVar futureBlocks
            utxoPool <- readTVar utxoPool

            -- try to link a new block to one of the recent blocks
            case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed future of
                BlockInserted fixed' lively' -> do
                    writeTVar recentBlocks lively'
                    writeTVar oldBlocks fixed'
                    return ("Received block was inserted into chain.", True)
                FutureBlock future'   -> do 
                    writeTVar futureBlocks future'
                    return ("Received block inserted into futures waiting list.", False)
                BlockAlreadyInserted -> return ("Received block was already present in the chain.", False)
                BlockInvalid         -> return ("Received block is invalid.", False)
                BlockNotLinked       -> return ("Received block can't be linked.", False)

            -- TODO: Could tag nodes in LivelyBlocks with UTXOPool-s collected up to a node - tradeof between speed and memory. <- probably not worth
        log logmsg

        -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
        when broadcastFurther $ do
            PeersList addresses <- readTVarIO peers
            forConcurrently_ addresses $ \address -> sendAndReceiveMsg (BlockMessage block) address (const $ return ())

    return ReceivedBlock

handleMessage _ _ appSt log sockaddr (TransactionMessage tx) = do 
    -- append new transaction to queue
    forkIO $ do
        atomically $ appendTransaction tx appSt
        log "handler: Received new transaction."

    return ReceivedTransaction

handleMessage _ _ (AppState {oldBlocks}) log sockaddr (QueryMessage query) = do
    log "Received a query."
    case query of
        -- This is pretty tragic as is indexing in a linked list
        BlockAtHeight n -> do
            blocks <- readTVarIO oldBlocks
            case blocks of
                FixedBlocks [] -> return (QueryAnswer NoBlockFound) 
                FixedBlocks (b : bs) ->
                    if blockBlockHeight b >= n then
                        case find (\x -> blockBlockHeight x == n) (b:bs) of
                            Just b -> do return (QueryAnswer (RequestedBlock b))
                            Nothing -> return (QueryAnswer NoBlockFound)
                    else
                        return (QueryAnswer NoBlockFound)


catchUpToBlockchain :: ForkMaxDiff
                    -> TargetHash
                    -> AppState
                    -> (String -> IO ())
                    -> IO ()
catchUpToBlockchain forkMaxDiff targetHash appSt@(AppState {recentBlocks, oldBlocks, futureBlocks, utxoPool, peers}) log = do
    lively   <- readTVarIO recentBlocks
    -- utxoPool is expected to be utxoPool of all txs from fixed
    (FixedBlocks fixed, utxoPool) <- atomically $ liftM2 (,) (readTVar oldBlocks) (readTVar utxoPool)
    future   <- readTVarIO futureBlocks
    (PeersList peersList) <- readTVarIO peers

    let n = 1 + case fixed of
            []  -> 0
            b:_ -> blockBlockHeight b

    maddresses <- sampleIO (min 8 (length peersList)) peersList
    case maddresses of
        Nothing -> log "Failed to query for new blocks. Not enough peers."
        Just addresses -> do 
            blocks <- concat <$> forConcurrently addresses (keepQuerying n)
            let (fixed', lively', future') = foldl' (update utxoPool) (FixedBlocks fixed, lively, future) blocks
            atomically $ do
                writeTVar recentBlocks lively'
                writeTVar futureBlocks future'
                writeTVar oldBlocks fixed'
            log "Succesfully queried for new blocks."


    where
        -- queries given address for blocks starting at height n and querying as long as we get valid answers.
        keepQuerying :: Integer -> Address -> IO [Block]
        keepQuerying n address = do
            sendAndReceiveMsg (QueryMessage $ BlockAtHeight n) address $ \case
                Just (QueryAnswer (RequestedBlock b))  -> (b :) <$> keepQuerying (n+1) address
                _ -> return []

        update :: UTXOPool -> (FixedBlocks, LivelyBlocks, FutureBlocks) -> Block -> (FixedBlocks, LivelyBlocks, FutureBlocks)
        update utxoPool (fixed, lively, future) block = 
            -- try to link a new block to one of the recent blocks
            case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed future  of
                BlockInserted fixed' lively' -> (fixed', lively', future)
                FutureBlock future'   -> (fixed, lively, future')
                _                     -> (fixed, lively, future)


    -- try to link a new block to one of the recent blocks
    -- case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed future of
    --     BlockInserted fixed' lively' -> do
    --         writeTVar recentBlocks lively'
    --         writeTVar oldBlocks fixed'
    --         return "Received block was inserted into chain."
    --     FutureBlock future'   -> do 
    --         writeTVar futureBlocks future'
    --         return "Received block inserted into futures waiting list."
    --     BlockAlreadyInserted -> return "Received block was already present in the chain."
    --     BlockInvalid         -> return "Received block is invalid."
    --     BlockNotLinked       -> return "Received block can't be linked."
    

-- move this somewhere more appriopriate (or don't)
difficultyToTargetHash :: Int -> TargetHash
difficultyToTargetHash n = TargetHash . RawHash . B.pack $ replicate n 0 ++ replicate (32-n) 255 

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
mining :: TargetHash -> AppState -> (String -> IO ()) -> IO ()
mining targetHash (AppState {recentBlocks, incomingTxs, peers}) log = forever $ do
    
    txs <- atomically $ do
        txs <- readTVar incomingTxs
        if null txs then
            retry
        else
            return txs

    lastblock <- getLastBlock <$> readTVarIO recentBlocks

    -- listen for new longest forks.
    -- Thread returns when there's been an update in recentBlocks 
    -- and now different block is a leaf furthest from root.
    let waitNewFork = atomically $ do
                newlastblock <- getLastBlock <$> readTVar recentBlocks
                check (shash256 (blockHeader newlastblock) /= shash256 (blockHeader lastblock))
            

    doMining lastblock (toList txs) `race_` threadDelay 240000000 `race_` waitNewFork

    where

        doMining lastblock txs = do
            timestamp <- getCurrentTime
            -- keys for coinbase money
            keys <- generateKeys
            let (ownedUTXO, block) = mineAfterBlock targetHash keys timestamp lastblock txs

            -- collect utxo in wallet:
            -- TODO
            log "We mined a coin!"
            -- _ ownedUTXO

            --  broadcast block
            (PeersList addresses) <- readTVarIO peers
            forConcurrently_ addresses $ \address -> sendAndReceiveMsg (BlockMessage block) address (const $ return ())


-- TODO: Who creates genesis block? is it part of running a node? 
-- IRL genesis and some initial contact list would be settled on outside the protocol or hardcoded into the protocol.

-- Recalculate UTXOPool on start or load it from file?
-- TODO: Load it ^ from file.

runNode :: IO ()
runNode = do
    CommandOptions configFilepath <- execParser parseCommand
    config <- fromJust <$> decodeFileStrict configFilepath -- unrecovarable errors here
    
    -- make logging function
    loggerlock <- newMVar ()
    let log = logger (loggingMode config) loggerlock

    let targetHash = difficultyToTargetHash $ targetDifficulty config

    -- TODO: optional cmd arg to load state from save, otherwise only loads

    mpeers      <- loadData (peersFilepath config)      `onException` log "app: Failed to load peers from file. Exits."
    mblockchain <- loadData (blockchainFilepath config) `onException` log "app: Failed to load blockchain from file. Exits."

    case mpeers of
        Nothing    -> log "app: Couldn't parse peers file."
        Just peers ->
            case mblockchain of
                Nothing -> log "app: Couldn't parse blockchain file."
                Just (FixedBlocks []) -> log "app: Received empty fixed blocks."
                Just (FixedBlocks (head : tail)) -> do 
                    
                    appState <- AppState <$> newTVarIO (LivelyBlocks $ newTree head)
                                         <*> newTVarIO (FixedBlocks tail) 
                                         <*> newTVarIO (FutureBlocks Map.empty)
                                         <*> newTVarIO Seq.empty
                                         <*> newTVarIO peers
                                         <*> newTVarIO (collectUTXOs Map.empty tail)  -- tail because UTXOPool of FixedBlocks - head is inside LivelyBlocks
                    
                    --
                    mining targetHash appState log 

                    -- 
                    -- TODO: catching up to blockchain
                    -- query for blocks after our last block

                    -- Idea connected with above ^ :
                    -- TODO: Enhance LivelyBlocks type to collect also future blocks i.e. blocks that might build on 
                    -- top of blocks we have not received yet.  

                    let serverAddr = Address "localhost" (port config)
                    forkIO $ server serverAddr log (serverHandler (handleMessage forkMaxDiff targetHash appState log) log)

                    return ()

    where
        -- how to open only a single handle but also have it closed automaticaly?
        --                             name to log under, message to log
        logger :: LoggingMode -> MVar () -> String -> IO ()
        logger mode lock str       = void $ forkIO $ withMVar lock (\a -> do
            time <- getCurrentTime
            let timeinfo = formatTime defaultTimeLocale "%z+0200" time -- timezonehoursToTimeZone 2
            let msg =  timeinfo ++ ": " ++ str
            case mode of 
                ToStdin     -> putStr msg
                ToFile path -> withFile path AppendMode (`hPutStr` msg)
            return a)
        
        serverHandler :: (SockAddr -> Message -> IO Answer) -> (String -> IO ()) -> HandlerFunc
        serverHandler handler log sockAddr msgbytes = do
            case decode msgbytes of 
                Nothing -> do 
                    log "handler: Unable to parse message."
                    return $ encode MessageParseError
                Just msg -> do
                    answer <-  handler sockAddr msg
                    return $ encode answer
            

