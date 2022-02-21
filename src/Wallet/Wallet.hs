{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Wallet.Wallet where
import Hashing (TargetHash, difficultyToTargetHash, shash256)
import BlockChain (ForkMaxDiff, LivelyBlocks, FutureBlocks, Lively (Lively), Future (Future), Fixed (Fixed))
import Network.Socket (ServiceName)
import Node (LoggingMode, withLogging, AppendFixed (appendFixed), PeersSet)
import Wallet.DBPool (onErrorLogAndQuit, PoolSettings, HasDB (executeDB), acquire)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, encodeFile)
import Server (Address(Address), server)
import MessageHandlers (toServerHandler)
import Wallet.Node (lightNodeHandler, lightNodeCatchUpToBlockchain, FixedLength (FixedLength))
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
import Wallet.DBTransaction (addFixedBlockHeader, selectFixedCount)
import Data.Foldable (for_)
import Hasql.Transaction (statement)
import qualified Hasql.Transaction.Sessions as Hasql
import Hasql.Transaction.Sessions (Mode(Write), IsolationLevel (Serializable))
import System.Exit (exitFailure)
import Control.Monad ((>=>))

data BlockchainConfig = BlockchainConfig {
    targetDifficulty :: Int,
    forkMaxDiff :: ForkMaxDiff,
    blockchainGenesis     :: Genesis
} deriving (Generic)

-- instance ToJSON BlockchainConfig
-- instance FromJSON BlockchainConfig

data NodeConfig = NodeConfig {
    port :: ServiceName,
    loggingMode :: LoggingMode,
    peersFilepath :: FilePath
} deriving (Generic)

-- instance ToJSON NodeConfig
-- instance FromJSON NodeConfig

data WalletConfig = WalletConfig {
    databaseConfig :: PoolSettings,
    blockchainConfig :: BlockchainConfig,
    nodeConfig :: NodeConfig
} deriving (Generic)

-- instance ToJSON WalletConfig
-- instance FromJSON WalletConfig

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
            -- log "app: Loaded peers and fixed blocks."

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
                 