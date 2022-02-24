{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Wallet.Wallet where
import Hashing (TargetHash, difficultyToTargetHash, shash256)
import BlockChain (ForkMaxDiff, LivelyBlocks, FutureBlocks, Lively (Lively), Future (Future), Fixed (Fixed))
import Network.Socket (ServiceName)
import Node (LoggingMode, withLogging, AppendFixed (appendFixed), PeersSet, Status)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON (parseJSON), eitherDecodeFileStrict, encodeFile, Value)
import Server (Address(Address), server)
import MessageHandlers (toServerHandler)
import Wallet.Node (lightNodeHandler, lightNodeCatchUpToBlockchain, FixedLength (FixedLength), HasDB (executeDB))
import BlockType (Genesis, BlockHeader (BlockHeader), Transaction, TXID, Cent, PublicAddress)
import Control.Concurrent.AdvSTM.TVar
import qualified Hasql.Pool as Pool
import Hasql.Pool (Pool)
import Control.Exception (bracket)
import qualified Data.Map as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently_)
import InMemory (HasLogging (logger), InMemory (readMemory, writeMemory, modifyMemory))
import Control.Concurrent.AdvSTM (AdvSTM, onCommit, atomically)
import Wallet.DBTransaction (addFixedBlockHeader, selectFixedCount)
import Data.Foldable (for_)
import Hasql.Transaction (statement)
import qualified Hasql.Transaction.Sessions as Hasql
import Hasql.Transaction.Sessions (Mode(Write), IsolationLevel (Serializable))
import System.Exit (exitFailure)
import Control.Monad ((>=>))
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.ByteString.Base64 as B64
import Data.Aeson.Types (Parser)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Wallet.Configs (PoolSettings (..), ConnectionSettings (..), WalletConfig(..), NodeConfig(..), BlockchainConfig(..))
import Hasql.Session (Session)
import Hasql.Connection (settings)
import BlockCreation (OwnedUTXO)

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
   Left  e -> log (show e) >> exitFailure
   Right a -> return a

-- Question: Can I recover from errors?

data Command response where 
    AddCoin         :: OwnedUTXO ->             Command AddCoinResponse
    AddTransaction  :: Transaction ->           Command AddTransactionResponse
    SendTransaction :: PublicAddress -> Cent -> Command SendTransactionResponse
    GetStatus       :: TXID ->                  Command StatusResponse

data AddCoinResponse
    = AddCoinSuccess
    | AddCoinFail

data AddTransactionResponse
    = AddTransactionSuccess
    | AddTransactionFail

data SendTransactionResponse
    = SendedTransaction
    | NotEnoughFunds
    | SendTransactionFailure

data StatusResponse
    = StatusIs TXID Status
    | GetStatusFailure



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
            -- concurrently_ _ (runServer log appState)
            runServer log appState

        initBlockchainState genesis fixedLength =
            BlockchainState genesis
                <$> newTVarIO (Lively (shash256 $ Left genesis) [])
                <*> newTVarIO (Future Map.empty)
                <*> newTVarIO (FixedLength fixedLength)
                 