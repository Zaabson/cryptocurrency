{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module App where

import Server (Address(..), server, HandlerFunc)
import BlockType (Block (blockHeader), Transaction, BlockHeader (timestamp))
import System.IO (withBinaryFile, IOMode (ReadMode, AppendMode), hPutStr, withFile, Handle)
import Data.Aeson (encode, decode, encodeFile, FromJSON, ToJSON, decodeFileStrict)
import qualified Data.ByteString.Lazy as LazyB (hGetContents)
import Control.Concurrent.STM (TVar, STM, modifyTVar', stateTVar, newTVarIO, readTVarIO, newTVar, readTVar, atomically, writeTVar, modifyTVar, retry, check, orElse)
import BlockChain (LivelyBlocks (LivelyBlocks), FixedBlocks (FixedBlocks), Blockchain(..), insertToChain, newTree, ForkMaxDiff(ForkMaxDiff), updateWithBlock, BlockchainUpdated (..), collectUTXOs, getLastBlock)
import qualified Data.Sequence as Seq (Seq, (|>), take, length, splitAt, empty)
import Control.Arrow (first)
import GHC.Generics (Generic)
import Options.Applicative
import Data.Maybe (fromJust)
import Control.Concurrent (MVar, forkIO, withMVar, newMVar, threadDelay)
import Control.Monad (void, forever)
import Data.Time (getCurrentTime, hoursToTimeZone, formatTime, defaultTimeLocale)
import Network.Socket (ServiceName, SockAddr)
import Message (Message (..), Answer(..))
import Control.Exception (onException)
import Hashing (TargetHash(TargetHash), RawHash(RawHash), shash256)
import qualified Data.ByteString as B
import Control.Monad.Reader (ReaderT)
import BlockValidation (UTXOPool)
import qualified Data.Map as Map
import BlockCreation (Keys(Keys), mineAfterBlock, OwnedUTXO (OwnedUTXO))
import qualified Codec.Crypto.RSA as RSA
import Crypto.Random.DRBG (newGenIO)
import Data.Foldable (toList)
import Control.Concurrent.Async (async, race, waitEitherCancel, waitSTM, race_)
import qualified Crypto.Random.DRBG as DRBG

-- data MiningMode
--     = Mining
--     | Idle

loadData :: FromJSON a => FilePath -> IO (Maybe a)
loadData path = withBinaryFile path ReadMode ((decode <$>) . LazyB.hGetContents) 

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
    incomingTxs  :: TVar (Seq.Seq Transaction),
    peers        :: TVar PeersList,
    utxoPool     :: TVar UTXOPool 
    }

appendTransaction :: Transaction -> AppState -> STM ()
appendTransaction txs AppState {..} = modifyTVar' incomingTxs (Seq.|> txs)

-- Take n transaction or fail if there is less
takeNTransactions :: Int -> AppState -> STM (Maybe (Seq.Seq Transaction))
takeNTransactions n AppState {..} = stateTVar incomingTxs $ \txs ->
    if Seq.length txs >= n then
        first Just $ Seq.splitAt n txs
    else
        (Nothing, txs)

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

handleMessage forkMaxDiff targetHash (AppState {recentBlocks, oldBlocks, utxoPool}) log sockaddr (BlockMessage block) = do
    
    -- do the validation etc concurrently not to hold a connection for long
    forkIO $ do
        logmsg <- atomically $ do 
            -- Note that this atomical operation is costly.

            lively   <- readTVar recentBlocks
            fixed    <- readTVar oldBlocks
            utxoPool <- readTVar utxoPool

            -- try to link a new block to one of the recent blocks
            case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed of
                BlockInserted fixed' lively' -> do
                    writeTVar recentBlocks lively'
                    writeTVar oldBlocks fixed'
                    return "Received block was inserted into chain."
                BlockAlreadyInserted -> return "Received block was already present in the chain."
                BlockInvalid         -> return "Received block is invalid."
                BlockNotLinked       -> return "Received block can't be linked."

            -- TODO: Could tag nodes in LivelyBlocks with UTXOPool-s collected up to a node - tradeof between speed and memory
        log logmsg

    return ReceivedBlock

handleMessage _ _ appSt log sockaddr (TransactionMessage tx) = do 
    -- append new transaction to queue
    forkIO $ do
        atomically $ appendTransaction tx appSt
        log "handler: Received new transaction."

    return ReceivedTransaction

-- move this somewhere more appriopraite (or don't)
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


-- This needs some more thinking. How to force lazy haskell to mine?
-- Also combining those waits will be tricky 
mining :: TargetHash -> AppState -> (String -> IO ()) -> IO ()
mining targetHash (AppState {recentBlocks, incomingTxs=incomingTxs}) log = forever $ do
    
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
            _ ownedUTXO

            --  broadcast block
            -- TODO
            _ block


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
                                         <*> newTVarIO Seq.empty
                                         <*> newTVarIO peers
                                         <*> newTVarIO (collectUTXOs Map.empty tail)  -- tail because UTXOPool of FixedBlocks - head is inside LivelyBlocks
                    
                    --
                    -- mining

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
            

