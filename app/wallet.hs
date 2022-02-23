{-# LANGUAGE OverloadedStrings #-}

import qualified Wallet.Wallet as Wallet 
import Wallet.Wallet ( runWallet )
import Node (LoggingMode(ToStdout, ToStderr))
import Wallet.Configs (PoolSettings (poolSize, timeout, PoolSettings), ConnectionSettings (..), WalletConfig(WalletConfig), NodeConfig(..), BlockchainConfig(..))
import qualified Wallet.Configs as Configs 
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Yaml (encodeFile)
import BlockChain (ForkMaxDiff(ForkMaxDiff))
import BlockType (Genesis(Genesis))
import Data.Yaml.Pretty (encodePretty, defConfig)
import qualified Data.ByteString as B

nodeConfig = NodeConfig {
    port = "49152",
    loggingMode = ToStderr,
    peersFilepath = "app/data/peers_wallet.json"
}

poolSettings = PoolSettings {
    poolSize=4,
    timeout=secondsToNominalDiffTime 30,
    Configs.connectionSettings=connectionSettings
}

connectionSettings = ConnectionSettings {
    dbhost="localhost", 
    dbport=5124,
    dbuser="postgres",
    dbpassword="",
    database=""
}

walletConfig = WalletConfig poolSettings blockchainConfig nodeConfig

blockchainConfig = BlockchainConfig 4 (ForkMaxDiff 3) (Genesis "Złoty")

-- main = runWallet undefined
main = encodeFile "wallet_config.yaml" walletConfig