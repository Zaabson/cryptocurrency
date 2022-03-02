{-# LANGUAGE DeriveGeneric, LambdaCase, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Node where
import Data.Aeson
import Data.Time
import GHC.Generics
import qualified Data.Map as Map
import Server
import Control.Concurrent.STM
import MessageType
import Control.Concurrent.Async (forConcurrently_, forConcurrently, Async, async, wait, withAsync)
import Control.Monad (when, void, forever, (>=>))
import Data.Function (on)
import Client (sendAndReceive)
import BlockChain (ForkMaxDiff)
import Hashing (TargetHash, difficultyToTargetHash)
import InMemory (InMemory (modifyMemoryIO, modifyMemory), HasLogging (logger), InMemoryRead (readMemoryIO), runAtomically)
import BlockType (Block, Genesis)
import Data.List (foldl')
import Numeric.Sampling (sampleIO)
import Network.Socket (ServiceName)
import BlockCreation (Keys (Keys))
import qualified Crypto.Random.DRBG as DRBG
import qualified Codec.Crypto.RSA as RSA
import Crypto.Random (CryptoRandomGen(newGenIO))
import Control.Concurrent (MVar, newMVar, forkIO, withMVar, threadDelay, takeMVar)
import Control.Monad.Except (runExceptT, ExceptT (ExceptT), withExceptT)
import GHC.IO.IOMode (IOMode(AppendMode))
import System.IO (withFile, hPutStrLn, hFlush, stdin, stdout, openFile, hClose, Handle, hPutStr, stderr, BufferMode (LineBuffering), hSetBuffering)
import Control.Concurrent.STM (newTQueue, writeTQueue)
import Control.Concurrent.STM.TMQueue
import Control.Exception (bracket, finally, handle, SomeException, throw)
import Data.Universe.Helpers (interleave)
import Hasql.Session (Session)
import qualified Hasql.Pool as Pool
import Text.Pretty.Simple (pShow)
import Data.Text.Lazy (unpack)
import System.Exit (exitFailure)
import Configs (PoolSettings (..), LoggingMode, ConnectionSettings (..), LoggingMode(..))
import Hasql.Pool (Pool)
import Hasql.Connection (settings)
import Data.Text.Encoding (encodeUtf8)

-- Collects functionality common between fullnode and wallet lightnode.
-- 

-- Constant
keyLength :: Int
keyLength = 2048

generateKeys :: IO Keys
generateKeys = do
    g <- newGenIO :: IO DRBG.HmacDRBG
    let (pub, priv, _) = RSA.generateKeyPair g keyLength
    return $ Keys pub priv

class AppendFixed appState m b where
    appendFixed :: appState -> [b] -> m () 

class HasDB appState where
    executeDBEither :: appState -> Session a -> IO (Either Pool.UsageError a)

executeDB :: (HasLogging appState, HasDB appState) =>
    appState -> Session a -> IO a
executeDB appState = onErrorLogAndQuit (logger appState) $ executeDBEither appState

-- Question: Can I recover from errors?
-- Answer: Yes in doMining (or at least we ignore the error and just go on).

onErrorLogAndQuit :: (String -> IO ()) -> (Session a -> IO (Either Pool.UsageError a)) -> (Session a -> IO a)
onErrorLogAndQuit log f = f >=> \case
   Left  e -> log ( unpack $ pShow e) >> exitFailure    -- lets go through intermediate text as we might want to swap log type to Text -> IO ()
   Right a -> return a

onErrorLogAndNothing :: String -> (String -> IO ()) -> (Session a -> IO (Either Pool.UsageError a)) -> (Session a -> IO (Maybe a))
onErrorLogAndNothing str log usePool = usePool >=> either (\e -> log (str <> unpack (pShow e)) >> return Nothing) (return . Just)

acquire :: PoolSettings -> IO Pool
acquire PoolSettings{connectionSettings=ConnectionSettings{..}, ..} =
    Pool.acquire (poolSize, timeout, settings (encodeUtf8 dbhost) dbport (encodeUtf8 dbuser) (encodeUtf8 dbpassword) (encodeUtf8 database))


sendAndReceiveMsg ::  Message -> (Maybe Answer -> IO a) -> Address -> IO a
sendAndReceiveMsg msg k address = sendAndReceive (encode msg) address (k . (>>=  decode))

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

insertPeer :: Address -> Status -> PeersSet -> PeersSet
insertPeer addr status (PeersSet s) = PeersSet (Map.insert addr status s)

-- Broadcast message to peers and update their statuses based on matching the expected answer.
-- broadcast :: TVar PeersSet -> Message -> Answer -> IO ()
broadcast :: InMemory appState m PeersSet => appState -> Message -> Answer -> IO ()
broadcast peers msg answer = do
    peersSet <- readMemoryIO peers
    forConcurrently_ (getAddresses peersSet) $ \addr ->
        sendAndReceiveMsg msg (expectAnswer peers addr answer) addr

-- Broadcast message to peers and update their statuses based on matching the expected answer.
-- This can delete some contacts so we run updatePeers afterwards to keep the number of contacts high.
-- broadcastAndUpdatePeers :: TVar PeersSet -> Message -> Answer -> IO ()
-- broadcastAndUpdatePeers :: TVar PeersSet -> Message -> Answer -> IO ()
broadcastAndUpdatePeers :: InMemory tvar m PeersSet => tvar -> Message -> Answer -> IO ()
broadcastAndUpdatePeers peers msg answer = do
    broadcast peers msg answer
    updatePeers peers

-- Broadcast message to peers and update their statuses based on matching the expected answer.
-- Also react to their answers with provided function.
-- broadcastWithReply :: TVar PeersSet -> Message -> Answer -> (Maybe Answer -> IO a) -> IO [a]
broadcastWithReply :: InMemory appState m PeersSet =>
    appState -> Message -> Answer -> (Maybe Answer -> IO b) -> IO [b]
broadcastWithReply peers msg answer f = do
    peersSet <- readMemoryIO peers
    forConcurrently (getAddresses peersSet) $ \addr ->
        sendAndReceiveMsg
            msg
            (\manswer -> do
                expectAnswer peers addr answer manswer
                f manswer)
            addr

-- broadcastWithReply_ :: TVar PeersSet -> Message -> Answer -> (Maybe Answer -> IO ()) -> IO ()
broadcastWithReply_ :: InMemory appState m PeersSet =>
    appState -> Message -> Answer -> (Maybe Answer -> IO b) -> IO ()
broadcastWithReply_ peers msg answer f = void $ broadcastWithReply peers msg answer f



-- If there's less than 10 addresses in our PeersSet ask all peers for their peers address lists.
-- Ping received addresses and add to PeersSet ones that responded.
-- updatePeers :: TVar PeersSet -> IO ()
updatePeers :: InMemory tvar m PeersSet => tvar -> IO ()
updatePeers peers = do
    PeersSet peersSet <- readMemoryIO peers
    when (Map.size peersSet < 10) $
        broadcastWithReply_ peers (ContactQueryMessage ContactQuery) (ContactQueryAnswer (ContactQueryResult [])) (\case
                Just (ContactQueryAnswer (ContactQueryResult xs)) -> pingAndAdd peers xs
                _ -> return ())

    where
        -- Ping addresses and add to PeersSet all that responded.
        -- pingAndAdd :: TVar PeersSet -> [Address] -> IO ()
        pingAndAdd peers addresses =
            forConcurrently_ addresses (\addr ->
                sendAndReceiveMsg
                    PingMessage
                    (\case
                        Just (PingAnswer ReceivedPing) -> runAtomically $ modifyMemory peers (\(PeersSet map) -> PeersSet $ Map.insert addr Active map)
                        _ -> return ())
                    addr)

-- To be used after broadcasting message to update PeersSet based on activity of the addressee's. 
expectAnswer :: (InMemory appState m PeersSet) -- collection of addresses and their activity status 
             => appState
             -> Address        -- addressee 
             -> Answer         -- the answer we expect
             -> Maybe Answer   -- the answer we got
             -> IO ()          -- updating of PeersSet according to address activity status
expectAnswer appState address expected answer = do
    time <- getCurrentTime
    modifyMemoryIO appState (\(PeersSet peers) ->
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


-- Constructor based equality for Answer type.
answerEqConstr :: Answer -> Answer -> Bool
answerEqConstr = geqConstr `on` from

-- Some code to derive answerEqConstr automatically:

-- The point is to save ourselves from a bug of not updating answerEqConstr with addition of new constructors.
-- Taken from "https://stackoverflow.com/questions/10112733/haskell-simple-constructor-comparison-function".
-- I went with Generics as it is already derived for Aeson.
class GEqC f where
  geqConstr :: f p -> f p -> Bool
  {-# INLINE geqConstr #-}
  geqConstr _ _ = True

instance GEqC f => GEqC (M1 i c f) where
  {-# INLINE geqConstr #-}
  geqConstr (M1 x) (M1 y) = geqConstr x y

instance GEqC (K1 i c)
instance GEqC (f :*: g)
instance GEqC U1
instance GEqC V1

instance (GEqC f, GEqC g) => GEqC (f :+: g) where
  {-# INLINE geqConstr #-}
  geqConstr (L1 x) (L1 y) = geqConstr x y
  geqConstr (R1 x) (R1 y) = geqConstr x y
  geqConstr _ _ = False


catchUpToBlockchain :: (HasLogging appState, InMemory appState m PeersSet)
    => ForkMaxDiff
    -> TargetHash
    -- -> (blockchain -> Block -> blockchain)   -- How to update blockchain with a new block
    -> (appState -> IO Integer)               -- For the initial blockchain, how to ask for the next block of interest. TODO: generalize to blockchain -> ContactQuery
    -> appState
    -> IO [Block]
catchUpToBlockchain forkMaxDiff targetHash whatsNextBlock appState = do
    -- get the length of the blockchain we have, we ask for the next block
    n <- whatsNextBlock appState
    -- n <- whatsNextBl 4ock

    -- get random selection of 1 to 8 addresses
    addresses <- getAddresses <$> readMemoryIO appState
    maddresses <- sampleIO (max 1 (min 8 (length addresses))) addresses

    case maddresses of
        Nothing -> do
            logger appState "Failed to query for new blocks. Not enough peers."
            return []
        Just addresses -> do
            logger appState "Queried for new blocks."
            -- interleave, so that blocks are added in order
            interleave <$> forConcurrently addresses (keepQuerying n)

    where
        -- TODO: refactor with broadcastWithReply 
        -- queries given address for blocks starting at height n and querying as long as we get valid answers.
        keepQuerying :: Integer -> Address -> IO [Block]
        keepQuerying n address = do
            sendAndReceiveMsg (BlockchainQueryMessage $ BlockAtHeight n) (\manswer -> do
                expectAnswer appState address (BlockchainQueryAnswer NoBlockFound) manswer
                case manswer of
                    Just (BlockchainQueryAnswer (RequestedBlock b))  -> (b :) <$> keepQuerying (n+1) address
                    _ -> return [] ) address


-- Load blockchain config + protocol config + node config + specific config

-- Load blockchain config = filepaths or data to connect to db
-- protocol config = TargetHash, ForkMaxDiff
-- node config = port, logging mode
-- fullnodeconfig = minerWaitForTxs and all thats left
-- walletconfig = same

-- this is some option, dont stick to it 
-- class LoadConfig c a where
--     load :: c -> IO a

-- -- ^ this is general, but for the ones below consider sticking with concrete type. (then main config could be made as record of smaller configs and thats fine) 

-- class ProtocolConfig c where
--     getTargetHash :: c -> TargetHash
--     getForkMaxDiff :: c -> ForkMaxDiff

-- class NodeConfig c where
--     getPort :: c -> ServiceName
--     getLoggingMode :: c -> LoggingMode

-- data ProtocolConfig = ProtocolConfig {
--     targetHash :: TargetHash,
--     forkMaxDiff :: ForkMaxDiff
-- }

-- data NodeConfig = NodeConfig {
--     port :: ServiceName,
--     loggingMode :: LoggingMode
-- }

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

makeLogger1 :: LoggingMode -> MVar () -> String -> IO ()
makeLogger1 Silent _ _ = return ()
makeLogger1 mode lock str       = void $ forkIO $ withMVar lock (\a -> do
    time <- getZonedTime
    -- TODO: fix time not displaying properly
    let timeinfo = formatTime defaultTimeLocale "%T" time
    let msg =  timeinfo ++ ": " ++ str
    case mode of
        ToStdout    -> putStrLn msg
        ToFile path -> withFile path AppendMode (`hPutStrLn` msg)
        ToStderr    -> hPutStrLn stderr msg
        Silent      -> return ()  -- covered above 
    return a)

makeLogger :: LoggingMode -> IO (String -> IO ())
makeLogger mode = makeLogger1 mode <$> newMVar ()

-- Helper for withLogging
withLoggingHdl :: ((String -> IO ()) -> IO a) -> Handle -> IO a
withLoggingHdl action hdl = do
    queue <- newTMQueueIO
    withAsync (hSetBuffering hdl LineBuffering >> logging hdl queue) (\as ->
        action (logHandle queue) `finally` quit hdl queue as
        )

    where
        -- Close logging queue, wait for remaining messages to be writen.
        quit hdl queue as = do
            atomically $ closeTMQueue queue
            wait as -- wait for queued logs to be logged

        -- Runs as async thread, logs messages from queue.
        logging hdl queue = do
            atomically (readTMQueue queue) >>= \case
                Nothing -> return ()
                Just str -> do
                    hPutStrLn hdl str
                    logging hdl queue

        -- Used in action to do logging, only appends to queue.
        logHandle :: TMQueue String -> String -> IO ()
        logHandle queue str = do
            time <- getZonedTime
            let logstr = formatTime defaultTimeLocale "%T" time ++ ": " ++ str
            atomically $ writeTMQueue queue logstr

-- Run action with provided logger handle. Logging appends message to a queue that is logged by an async thread.
-- At quit the queue is closed and remaining messages are logged.
withLogging :: LoggingMode -> ((String -> IO ()) -> IO a) -> IO a
withLogging Silent action = action (const $ return ())
withLogging (ToFile fp) action = withFile fp AppendMode $ withLoggingHdl action
withLogging ToStdout action = withLoggingHdl action stdout
withLogging ToStderr action = withLoggingHdl action stderr


-- On error log error and rethrow. 
-- Print quitMsg.
topLevelErrorLog :: String -> (String -> IO ()) -> IO a -> IO a
topLevelErrorLog quitMsg log io = logError `finally` log quitMsg
    where
        logError = handle (\(e :: SomeException) -> log (show e) >> throw e) io


-- this type is not very usefull in general, incomingTxs and minerWallet only apply to fullnode, PeerSet is required by class constraint and blockchainState is variable.
-- data AppState blobkchain = AppState {
--     blockchainState :: blockchain,   
--     incomingTxs  :: TVar (Seq.Seq Transaction),
--     peers        :: TVar PeersSet,
--     minerWallet  :: TVar SimpleWallet 
--     }


-- Consider abondoning RunningApp idea. Maybe better withLaunchedApp :: (IO Appstate) -> (AppState -> IO a) -> IO a

-- appState, running main (mining, server), logging function
newtype RunningApp appState = RunningApp (appState, Async ())
-- maybe logging inside appState as already functions expect HasLogging appState


