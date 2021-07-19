{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App where

import Server (Address(..), server, HandlerFunc)
import BlockType (Block (blockHeader), Transaction, BlockHeader (timestamp), blockBlockHeight, Genesis (Genesis))
import System.IO (withBinaryFile, IOMode (ReadMode, AppendMode), hPutStr, withFile, Handle)
import Data.Aeson (encode, decode, encodeFile, FromJSON, ToJSON, decodeFileStrict, eitherDecodeFileStrict)
import qualified Data.ByteString.Lazy as LazyB (hGetContents)
import Control.Concurrent.STM (TVar, STM, modifyTVar', stateTVar, newTVarIO, readTVarIO, newTVar, readTVar, atomically, writeTVar, modifyTVar, retry, check, orElse)
import BlockChain (LivelyBlocks (LivelyBlocks), FixedBlocks (FixedBlocks), Blockchain(..), insertToChain, newTree, ForkMaxDiff(ForkMaxDiff), updateWithBlock, BlockchainUpdated (..), collectUTXOs, getLastBlock, FutureBlocks (FutureBlocks))
import qualified Data.Sequence as Seq (Seq, (|>), take, length, splitAt, empty)
import Control.Arrow (first, Arrow ((&&&), (***)))
import GHC.Generics (Generic)
import Options.Applicative
import Data.Maybe (fromJust)
import Control.Concurrent (MVar, forkIO, withMVar, newMVar, threadDelay)
import Control.Monad (void, forever, liftM2, when, join, liftM3)
import Data.Time (getCurrentTime, hoursToTimeZone, formatTime, defaultTimeLocale)
import Network.Socket (ServiceName, SockAddr, Family (AF_802))
import Message (Message (..), Answer(..), Query (BlockAtHeight), QueryResult (NoBlockFound, RequestedBlock))
import Control.Exception (onException)
import Hashing (TargetHash(TargetHash), RawHash(RawHash), shash256)
import qualified Data.ByteString as B
import Control.Monad.Reader (ReaderT)
import BlockValidation (UTXOPool, UTXO (UTXO), validateBlock)
import qualified Data.Map as Map
import BlockCreation (Keys(Keys), mineAfterBlock, OwnedUTXO (OwnedUTXO))
import qualified Codec.Crypto.RSA as RSA
import Crypto.Random.DRBG (newGenIO)
import Data.Foldable (toList, find, Foldable (foldl'))
import Control.Concurrent.Async (async, race, waitEitherCancel, waitSTM, race_, forConcurrently_, forConcurrently, concurrently_)
import qualified Crypto.Random.DRBG as DRBG
import Client (sendToAll, send, sendAndReceive)
import Numeric.Sampling (sampleIO)

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


-- LivelyBlocks by definition contains at least one block, so we differentiate between the empty case.
-- The invariant is that root of LivelyBlocks is the next block after the head of FixedBlocks.
-- Also UTXOPool is a pool of txs up of all FixedBlocks.
type BlockchainState = Either Genesis BlockchainState'
    
data BlockchainState' = BlockchainState' (TVar FixedBlocks) (TVar LivelyBlocks) (TVar FutureBlocks) (TVar UTXOPool) Genesis

readFixedBlocks :: BlockchainState' -> STM FixedBlocks
readFixedBlocks (BlockchainState' fixed _ _ _ _) = readTVar fixed

readLivelyBlocks :: BlockchainState' -> STM LivelyBlocks
readLivelyBlocks (BlockchainState' _ lively _ _ _) = readTVar lively

readFutureBlocks :: BlockchainState' -> STM FutureBlocks
readFutureBlocks (BlockchainState' _ _ future _ _) = readTVar future

readUTXOPool :: BlockchainState' -> STM UTXOPool
readUTXOPool (BlockchainState' _ _ _ utxoPool _) = readTVar utxoPool

writeFixedBlocks :: BlockchainState' -> FixedBlocks -> STM ()
writeFixedBlocks (BlockchainState' fixed _ _ _ _) = writeTVar fixed

writeLivelyBlocks :: BlockchainState' -> LivelyBlocks -> STM ()
writeLivelyBlocks (BlockchainState' _ lively _ _ _) = writeTVar lively

writeFutureBlocks :: BlockchainState' -> FutureBlocks -> STM ()
writeFutureBlocks (BlockchainState' _ _ future _ _) = writeTVar future

writeUTXOPool :: BlockchainState' -> UTXOPool -> STM ()
writeUTXOPool (BlockchainState' _ _ _ utxoPool _) = writeTVar utxoPool



-- probably unneeded? Genesis is only important for empty blockchain 
-- getGenesis :: BlockchainState' -> Genesis
-- getGenesis BlockchainState' _ _ _ _ genesis = genesis

-- TODO: Investigate all the places where I forgot about genesis.

data AppState = AppState {
    blockchainState :: TVar BlockchainState,    
    incomingTxs  :: TVar (Seq.Seq Transaction),
    peers        :: TVar PeersList
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

-- takes block and create the BlockchainState' for a single block
initBlockchainState :: Genesis -> Block -> STM BlockchainState'
initBlockchainState genesis block = do
    ($ genesis) <$> 
        (BlockchainState' <$> newTVar (FixedBlocks [])
                          <*> newTVar (LivelyBlocks $ newTree block)
                          <*> newTVar (FutureBlocks Map.empty)
                          <*> newTVar Map.empty)

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

handleMessage forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) log sockaddr (BlockMessage block) = do
    
    -- do the validation etc concurrently not to hold a connection for long
    forkIO . join . atomically $ do 
        blockchainSt <- readTVar blockchainState
        either toGenesis toBlockchain blockchainSt

    -- forkIO $ either blockchainState toGenesis toBlockchain
                
    return ReceivedBlock

    where 

        toGenesis genesis = case validateBlock targetHash Map.empty block of
            (True, _) -> do
                blockchainState' <- initBlockchainState genesis block
                writeTVar blockchainState (Right blockchainState')
                return $ log "Received first block after genesis."
            (False, _) -> return $ log "Received invalid block linking to genesis."


        toBlockchain blockchainState' = do
            -- Note that this atomical operation is time-consuming.

            lively   <- readLivelyBlocks blockchainState'
            fixed    <- readFixedBlocks blockchainState'
            future   <- readFutureBlocks blockchainState'
            utxoPool <- readUTXOPool blockchainState'

            -- try to link a new block to one of the recent blocks
            case updateWithBlock forkMaxDiff targetHash utxoPool block lively fixed future of
                BlockInserted fixed' lively' utxoPool' -> do
                    writeLivelyBlocks blockchainState' lively'
                    writeFixedBlocks blockchainState' fixed'
                    writeUTXOPool blockchainState' utxoPool'
                    return $ do
                        log "Received block was inserted into chain."
                        -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                        PeersList addresses <- readTVarIO peers
                        forConcurrently_ addresses $ \address -> sendAndReceiveMsg (BlockMessage block) address (const $ return ())            
                FutureBlock future'   -> do
                    writeFutureBlocks blockchainState' future'
                    return $ do 
                        log "handler: Received block inserted into futures waiting list."
                        -- We received a block that doesn't link to a known recent chain. Let's query for new blocks.
                        catchUpToBlockchain forkMaxDiff targetHash appSt log
                BlockAlreadyInserted -> return $ log "handler: Received block was already present in the chain."
                BlockInvalid         -> return $ log "handler: Received block is invalid."
                -- BlockNotLinked       -> return $ log "Received block can't be linked."
                

handleMessage _ _ appSt log sockaddr (TransactionMessage tx) = do 
    -- append new transaction to queue
    forkIO $ do
        atomically $ appendTransaction tx appSt
        log "handler: Received new transaction."

    return ReceivedTransaction

handleMessage _ _ (AppState {blockchainState}) log sockaddr (QueryMessage query) = do
    log "handler: Received a query."
    case query of
        -- This is pretty tragic as is indexing in a linked list
        BlockAtHeight n -> do
            blocks <- atomically $ readTVar blockchainState >>= either (const $ return (FixedBlocks [])) readFixedBlocks
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
catchUpToBlockchain forkMaxDiff targetHash appSt@(AppState {blockchainState, peers}) log = do
    blocks <- queryForBlocks
    atomically $ do
        readTVar blockchainState

        -- !!!
        -- How do we represent a state of many blocks linking to genesis?
        -- LivelyBlocks has a single root.

            -- let (utxoPool', fixed', lively', future') = foldl' update (utxoPool, FixedBlocks fixed, lively, future) blocks
            -- atomically $ do
            --     writeLivelyBlocks blockchainState lively'
            --     writeFutureBlocks blockchainState future'
            --     writeFixedBlocks blockchainState fixed'
            --     writeUTXOPool blockchainState utxoPool' 

            -- log "Succesfully queried for new blocks."

            -- (FixedBlocks fixed, utxoPool, lively) <- liftM3 (_,_,_) (readFixedBlocks blockchainState') (readUTXOPool blockchainState') (readLivelyBlocks blockchainState')
            -- future <-  atomically $ readFutureBlocks blockchainState'
            -- (PeersList peersList) <- readTVarIO peers


    where
        -- queries given address for blocks starting at height n and querying as long as we get valid answers.
        keepQuerying :: Integer -> Address -> IO [Block]
        keepQuerying n address = do
            sendAndReceiveMsg (QueryMessage $ BlockAtHeight n) address $ \case
                Just (QueryAnswer (RequestedBlock b))  -> (b :) <$> keepQuerying (n+1) address
                _ -> return []
        
        queryForBlocks :: IO [Block]
        queryForBlocks = do
            -- get the length of the blockchain we have, we ask for the next block
            n <- atomically $ do
                readTVar blockchainState >>= \case
                    Left genesis -> return 1
                    Right blockchainState' -> do
                        FixedBlocks fixed <-readFixedBlocks blockchainState'
                        return $ 1 + case fixed of
                                        []  -> 0
                                        b:_ -> blockBlockHeight b

            -- get random selection of up to 8 addresses
            (PeersList peersList) <- readTVarIO peers
            maddresses <- sampleIO (min 8 (length peersList)) peersList

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
                BlockInserted fixed' lively' utxoPool' -> (utxoPool', fixed', lively', future')
                FutureBlock future'   -> (utxoPool, fixed, lively, future')
                _                     -> (utxoPool, fixed, lively, future)


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
mining targetHash (AppState {blockchainState, incomingTxs, peers}) log = forever $ do
    
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
    eitherConfig <- eitherDecodeFileStrict configFilepath
    case eitherConfig of
        Left error -> do
            print error
            print "Unable to read config file. Quits."
        Right config -> do
        -- config <- fromJust <$> decodeFileStrict configFilepath -- unrecovarable errors here
            
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
                Left err    -> do print err
                                  print "Couldn't open or parse peers file. Quits."
                Right peers ->
                    case eitherBlockchain of
                        -- Nothing -> log "app: Couldn't parse blockchain file."
                        Left err -> do print err
                                       print "Failed to load blockchain from file. Quits."
                        Right (FixedBlocks []) -> do 
                            -- TODO: read Genesis block from file.
                            -- TODO: fix first block mining.
                            log "app: Received empty fixed blocks."
                        Right (FixedBlocks (head : tail)) -> do 
                            
                            appState <- AppState <$> newTVarIO (LivelyBlocks $ newTree head)
                                                <*> newTVarIO (FixedBlocks tail) 
                                                <*> newTVarIO (FutureBlocks Map.empty)
                                                <*> newTVarIO Seq.empty
                                                <*> newTVarIO peers
                                                <*> newTVarIO (collectUTXOs Map.empty tail)  -- tail because UTXOPool of FixedBlocks - head is inside LivelyBlocks
                            
                            -- TODO: catching up to blockchain
                            -- query for blocks after our last block
                            forkIO $ catchUpToBlockchain forkMaxDiff targetHash appState log

                            let mine = mining targetHash appState log 

                            let serverAddr = Address "localhost" (port config)
                            let runServer = server serverAddr log (serverHandler (handleMessage forkMaxDiff targetHash appState log) log)

                            concurrently_ mine runServer

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
            

