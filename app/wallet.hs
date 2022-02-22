{-# LANGUAGE OverloadedStrings #-}

import qualified Wallet.Wallet as Wallet 
import Wallet.Wallet ( runWallet )
import Node (LoggingMode(ToStdout))
import Wallet.Configs (PoolSettings (poolSize, timeout, PoolSettings), ConnectionSettings (..), WalletConfig(WalletConfig), NodeConfig(..), BlockchainConfig(..))
import qualified Wallet.Configs as Configs 
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Aeson (encodeFile)
import BlockChain (ForkMaxDiff(ForkMaxDiff))
import BlockType (Genesis(Genesis))

nodeConfig = NodeConfig {
    port = "5024",
    loggingMode = ToStdout,
    peersFilepath = "app/data/peers.json"
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
    database="data"
}

walletConfig = WalletConfig poolSettings blockchainConfig nodeConfig

blockchainConfig = BlockchainConfig 4 (ForkMaxDiff 2) (Genesis "Złoty")

-- main = runWallet undefined
main = encodeFile "wallet_config.json" walletConfig