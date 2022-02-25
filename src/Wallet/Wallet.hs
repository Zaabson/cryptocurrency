{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Wallet.Wallet where
import Hashing (TargetHash, difficultyToTargetHash, shash256)
import BlockChain (ForkMaxDiff, LivelyBlocks, FutureBlocks, Lively (Lively), Future (Future), Fixed (Fixed))
import Network.Socket (ServiceName, Socket)
import Node (LoggingMode, withLogging, AppendFixed (appendFixed), PeersSet, Status)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON (parseJSON), eitherDecodeFileStrict, encodeFile, Value, decode)
import Server (Address(Address), server)
import MessageHandlers (toServerHandler)
import Wallet.Node (lightNodeHandler, lightNodeCatchUpToBlockchain, FixedLength (FixedLength), HasDB (executeDB))
import BlockType (Genesis, BlockHeader (BlockHeader))
import Control.Concurrent.AdvSTM.TVar
import qualified Hasql.Pool as Pool
import Hasql.Pool (Pool)
import Control.Exception (bracket)
import qualified Data.Map as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently_)
import InMemory (HasLogging (logger), InMemory (readMemory, writeMemory, modifyMemory))
import Control.Concurrent.AdvSTM (AdvSTM, onCommit, atomically)
import Wallet.Session (addFixedBlockHeader, selectFixedCount, selectStatus, insertTransaction, insertOwnedKeys, selectOwnedByStatus)
import Data.Foldable (for_)
import Hasql.Transaction (statement)
import qualified Hasql.Transaction.Sessions as Hasql
import Hasql.Transaction.Sessions (Mode(Write), IsolationLevel (Serializable))
import System.Exit (exitFailure)
import Control.Monad ((>=>))
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Text.Pretty.Simple (pShow)
import Data.Aeson.Types (Parser)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Wallet.Configs (PoolSettings (..), ConnectionSettings (..), WalletConfig(..), NodeConfig(..), BlockchainConfig(..))
import Hasql.Session (Session)
import Hasql.Connection (settings)
import Wallet.Repl (serveRepl, CommandR(..), AddCoinResponse (AddCoinFail, AddCoinSuccess), AddTransactionResponse (AddTransactionSuccess, AddTransactionFail), SendTransactionResponse (SendTransactionFailure, NotEnoughFunds))
import Wallet.Type (StoredTransaction(StoredTransaction), Status (Waiting, Validated))
import BlockCreation (Keys(Keys), OwnedUTXO (OwnedUTXO), createSendingTransaction)
import BlockValidation (UTXO(UTXO))

data BlockchainState = BlockchainState {
    getGenesis :: Genesis,
    getLively :: TVar (Lively BlockHeader),
    getFuture :: TVar (Future BlockHeader),
    getFixedLength :: TVar FixedLength
    -- Fixed is kept in database.
}

data AppState = AppState {
    blockchainState :: BlockchainState,
    getLogger :: String -> IO (),
    getDBPool :: Pool,
    getContacts :: TVar PeersSet
}

-- Option1 : 
--  - appState contains (Session a -> IO a), but it's valid only in some scope (same as logging but whatever)
-- Option2 : 
--  - appState contains pool
-- Option 3:
--  - withPool :: HasDB appState => PoolSettings -> (appState -> IO a) -> appState -> IO a  

instance InMemory AppState AdvSTM (Future BlockHeader) where
    readMemory = readMemory . getFuture . blockchainState
    writeMemory = writeMemory . getFuture . blockchainState
    modifyMemory = modifyMemory . getFuture . blockchainState

instance InMemory AppState AdvSTM (Lively BlockHeader) where
    readMemory = readMemory . getLively . blockchainState
    writeMemory = writeMemory . getLively . blockchainState
    modifyMemory = modifyMemory . getLively . blockchainState

instance InMemory AppState AdvSTM PeersSet where
    readMemory = readMemory . getContacts
    writeMemory = writeMemory . getContacts
    modifyMemory = modifyMemory . getContacts

instance InMemory AppState AdvSTM FixedLength  where
    readMemory = readMemory . getFixedLength . blockchainState
    writeMemory = writeMemory . getFixedLength . blockchainState
    modifyMemory = modifyMemory . getFixedLength . blockchainState


instance AppendFixed AppState AdvSTM BlockHeader where
    appendFixed appState newfixed = 
        onCommit . executeDB appState . Hasql.transaction Serializable Write $ for_ newfixed addFixedBlockHeader

instance HasDB AppState where 
    executeDB appState = onErrorLogAndQuit (getLogger appState) $ Pool.use (getDBPool appState)  
-- Turned out in one place I do want to recover from error, in repl where we execute user provided commands.

instance HasLogging AppState where
    logger = getLogger 

-- Version for AdvSTM.
-- Load JSON data and save it on quit.
-- Error can be thrown at loading and then just quit.
withLoadSave :: (FromJSON a, ToJSON a) => FilePath -> (Either String (TVar a) -> IO b) -> IO b
withLoadSave fp = bracket
    (do
        eload <- eitherDecodeFileStrict fp
        either (return . Left) (fmap Right . newTVarIO) eload)
    (either (const $ return ()) (atomically . readTVar >=> encodeFile fp))

acquire :: PoolSettings -> IO Pool
acquire PoolSettings{connectionSettings=ConnectionSettings{..}, ..} =
    Pool.acquire (poolSize, timeout, settings (encodeUtf8 dbhost) dbport (encodeUtf8 dbuser) (encodeUtf8 dbpassword) (encodeUtf8 database))


onErrorLogAndQuit :: (String -> IO ()) -> (Session a -> IO (Either Pool.UsageError a)) -> (Session a -> IO a)
onErrorLogAndQuit log f = f >=> \case
   Left  e -> log ( unpack $ pShow e) >> exitFailure    -- lets go through intermediate text as we might want to swap log type to Text -> IO ()
   Right a -> return a

-- Question: Can I recover from errors?



-- Execute user's wallet command provided with a handle to db pool that logs the error and projects to Nothing.
replHandler :: (forall a . Session a -> IO (Maybe a)) -> CommandR r-> IO r
-- replHandler :: appState -> CommandR r-> IO r
replHandler usePool (AddCoin (OwnedUTXO (UTXO txid vout _) (Keys pub priv))) = do 
    -- we discard Output information from OwnedUTXO 
    m <- usePool $ insertOwnedKeys txid (fromInteger vout) pub priv
    case m of 
        Nothing -> return AddCoinFail
        Just () -> return AddCoinSuccess 

replHandler usePool (AddTransaction tx blockref) = do 
    m <- usePool $ insertTransaction (StoredTransaction (shash256 tx) blockref tx Waiting)
    case m of 
        Nothing -> return AddTransactionSuccess 
        Just () -> return AddTransactionFail

-- replHandler usePool (SendTransaction recipient n) = do
--     mutxos <- usePool $ selectOwnedByStatus Validated
--     case mutxos of 
--         Nothing -> return SendTransactionFailure
--         Just utxos ->
--             let (newutxo, usedUTXOs, newtx) = createSendingTransaction utxos newkeys recipient n
--                 in insertOwnedKeys txid (fromInteger vout) pub priv

replHandler usePool (SendTransaction recipient n) = _ . usePool $ do
    utxos <- selectOwnedByStatus Validated
    case createSendingTransaction utxos newkeys recipient n of
        Nothing -> return NotEnoughFunds 
        Just (newutxo, _, newtx) -> 
            -- Here only remove
            insertOwnedKeys txid (fromInteger vout) pub priv
    _


replHandler usePool (GetStatus txid) = _ $ selectStatus txid

runWallet :: WalletConfig  -> IO ()
runWallet config =
    withLogging (loggingMode $ nodeConfig config) $ \log ->
        withLoadSave (peersFilepath $ nodeConfig config) $ \case
            Left e -> log e >> exitFailure
            Right peers ->
                bracket (acquire $ databaseConfig config) Pool.release $ \pool ->
                    main log pool peers

    where
        targetHash = difficultyToTargetHash . targetDifficulty . blockchainConfig $ config
        forkMaxDiff1 = forkMaxDiff $ blockchainConfig config
        serverAddr = Address "localhost" (port $ nodeConfig config)
        replAddr   = Address "localhost" (replPort config)
        runServer log appState = server serverAddr log (toServerHandler (lightNodeHandler forkMaxDiff1 targetHash appState) log)

        main log pool peers = do
            log "wallet: Started."

            -- Read FixedLength
            fixedLength <- onErrorLogAndQuit log (Pool.use pool) selectFixedCount

            blockchainState <- initBlockchainState (blockchainGenesis $ blockchainConfig config) fixedLength
            let appState = AppState blockchainState log pool peers

            -- query for blocks after our last block
            forkIO $ lightNodeCatchUpToBlockchain forkMaxDiff1 targetHash appState

            -- forkIO runServer
            -- forkIO mine
            concurrently_ (serveRepl replAddr log _) (runServer log appState)
            runServer log appState

        initBlockchainState genesis fixedLength =
            BlockchainState genesis
                <$> newTVarIO (Lively (shash256 $ Left genesis) [])
                <*> newTVarIO (Future Map.empty)
                <*> newTVarIO (FixedLength fixedLength)
