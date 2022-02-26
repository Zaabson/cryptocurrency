{-# LANGUAGE DeriveGeneric #-}

module Configs where

import Network.Socket (ServiceName)
import GHC.Generics (Generic)
import Data.Time.Clock (NominalDiffTime)
import Data.Text (Text)
import Data.Binary (Word16)
import BlockChain (ForkMaxDiff)
import BlockType (Genesis)
import Data.Aeson (FromJSON, ToJSON)

data LoggingMode = ToFile FilePath | ToStdout | ToStderr | Silent deriving Generic
instance ToJSON LoggingMode
instance FromJSON LoggingMode


data NodeConfig = NodeConfig {
    port  :: ServiceName,
    loggingMode  ::  LoggingMode,
    peersFilepath  ::  FilePath
} deriving (Generic)

data PoolSettings = PoolSettings {
    poolSize :: Int,
    timeout :: NominalDiffTime,
    connectionSettings :: ConnectionSettings
} deriving (Generic)

data ConnectionSettings =  ConnectionSettings {
    dbhost :: Text, 
    dbport :: Word16,
    dbuser :: Text,
    dbpassword :: Text,
    database :: Text
} deriving (Generic)

data BlockchainConfig = BlockchainConfig {
    targetDifficulty :: Int,
    forkMaxDiff ::  ForkMaxDiff,
    blockchainGenesis :: Genesis
} deriving (Generic)

data WalletConfig = WalletConfig {
    databaseConfig :: PoolSettings,
    blockchainConfig :: BlockchainConfig,
    nodeConfig :: NodeConfig,
    replPort :: ServiceName
} deriving (Generic)

instance ToJSON NodeConfig
instance FromJSON NodeConfig

instance ToJSON  PoolSettings
instance FromJSON PoolSettings

instance ToJSON  ConnectionSettings
instance FromJSON ConnectionSettings

instance ToJSON  BlockchainConfig
instance FromJSON BlockchainConfig

instance ToJSON  WalletConfig
instance FromJSON WalletConfig