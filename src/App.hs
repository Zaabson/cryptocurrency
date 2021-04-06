{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module App where

import Server (Address(..), server, HandlerFunc)
import BlockType (Block, Transaction)
import System.IO (withBinaryFile, IOMode (ReadMode, AppendMode), hPutStr, withFile, Handle)
import Data.Aeson (encode, decode, encodeFile, FromJSON, ToJSON, decodeFileStrict)
import qualified Data.ByteString.Lazy as LazyB (hGetContents)
import Control.Concurrent.STM (TVar, STM, modifyTVar', stateTVar, newTVarIO, readTVarIO, newTVar, readTVar, atomically, writeTVar, modifyTVar)
import BlockChain (LivelyBlocks (LivelyBlocks), FixedBlocks (FixedBlocks), Blockchain(..), insertToChain)
import qualified Data.Sequence as Seq (Seq, (|>), take, length, splitAt, empty)
import Control.Arrow (first)
import GHC.Generics (Generic)
import Options.Applicative
import Data.Maybe (fromJust)
import Control.Concurrent (MVar, forkIO, withMVar, newMVar)
import Control.Monad (void)
import Data.Time (getCurrentTime, hoursToTimeZone, formatTime, defaultTimeLocale)
import Network.Socket (ServiceName, SockAddr)
import Message (Message (..), Answer(..))

data MiningMode
    = Mining
    | Idle

loadData :: FromJSON a => FilePath -> IO (Maybe a)
loadData path = withBinaryFile path ReadMode ((decode <$>) . LazyB.hGetContents) 

-- saveBlockChain == Data.Aeson.encodeFile
-- loadBlockchain == Data.Aeson.decodeFileStrict

data Save = Save LivelyBlocks (Seq.Seq Transaction)
    deriving (Show, Generic)

instance ToJSON Save 
instance FromJSON Save

-- probably will be used for manual testing
--             Fixedblocks path, Save path
loadSavedState :: FilePath -> FilePath -> FilePath -> IO (Maybe AppState)
loadSavedState fixedpath savepath networkpath = do 
    mfixed <- loadData fixedpath
    msave <- loadData savepath
    mnetwork <- loadData networkpath
    case (mfixed, msave, mnetwork) of
        (Just fixed, Just (Save lively txs), Just peers) -> do
            recentBlocks <- newTVarIO lively
            oldBlocks    <- newTVarIO fixed
            incomingTxs  <- newTVarIO txs
            peers'        <- newTVarIO peers
            return $ Just $ AppState {recentBlocks = recentBlocks, 
                                      oldBlocks    = oldBlocks,
                                      incomingTxs  = incomingTxs,
                                      peers        = peers' }
        _ -> return Nothing

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
    peers        :: TVar PeersList
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

-- "These" instead of "Either" ? 
data LoggingMode = ToFile FilePath | ToStdin deriving Generic
instance ToJSON LoggingMode
instance FromJSON LoggingMode


data Config = Config {
        blockchainFilepath :: FilePath,
        targetDifficulty   :: Int,
        peersFilepath :: FilePath,
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

handleMessage :: AppState -> (String -> IO ()) -> SockAddr -> Message -> IO Answer
handleMessage appSt log sockaddr PingMessage = do
    -- TODO: here should update list of peers
    log "handler: Received Ping."
    return AnswerPing

handleMessage (AppState {recentBlocks}) log sockaddr (BlockMessage block) = do
    
    logmsg <- atomically $ do 
        LivelyBlocks lively <- readTVar recentBlocks
        -- try to link a new block to one of the more recent blocks
        case insertToChain block lively of 

            Nothing -> return "handler: Failed to connect block to blockchain."

            Just lively' -> do 
                writeTVar recentBlocks (LivelyBlocks lively')
                return "handler: Received block inserted to blockchain."
    
    log logmsg

    return ReceivedBlock

handleMessage appSt log sockaddr (TransactionMessage tx) = do 
    -- append new transaction to queue
    atomically $ appendTransaction tx appSt
    log "handler: Received new transaction."
    return ReceivedTransaction


runNode :: IO ()
runNode = do
    CommandOptions configFilepath <- execParser parseCommand
    config <- fromJust <$> decodeFileStrict configFilepath -- unrecovarable errors here
    
    -- make logging function
    loggerlock <- newMVar ()
    let log = logger (loggingMode config) loggerlock

    -- TODO: optional cmd arg to load state from save, otherwise only loads

    let serverAddr = Address "localhost" (port config)
    -- forkIO $ server serverAddr log (serverHandler appSt log) 
    
    

    undefined 

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
        
        serverHandler :: AppState -> (String -> IO ()) -> HandlerFunc
        serverHandler appSt log sockAddr msgbytes = do
            case decode msgbytes of 
                Nothing -> do 
                    log "handler: Unable to parse message."
                    return $ encode MessageParseError
                Just msg -> do
                    answer <- handleMessage appSt log sockAddr msg
                    return $ encode answer
            

