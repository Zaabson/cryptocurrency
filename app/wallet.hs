{-# LANGUAGE OverloadedStrings #-}

import qualified Wallet.Wallet as Wallet 
import Wallet.Wallet ( runWallet )
import Node (LoggingMode(ToStdout, ToStderr))
import Wallet.Configs (PoolSettings (poolSize, timeout, PoolSettings), ConnectionSettings (..), WalletConfig(WalletConfig), NodeConfig(..), BlockchainConfig(..))
import qualified Wallet.Configs as Configs 
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Yaml (encodeFile, decodeFileEither, prettyPrintParseException)
import BlockChain (ForkMaxDiff(ForkMaxDiff))
import BlockType (Genesis(Genesis))
import Data.Yaml.Pretty (encodePretty, defConfig)
import qualified Data.ByteString as B
import Options.Applicative
import Control.Exception (onException)
import System.Exit (exitFailure)

-- nodeConfig = NodeConfig {
--     port = "49152",
--     loggingMode = ToStderr,
--     peersFilepath = "app/data/peers_wallet.json"
-- }

-- poolSettings = PoolSettings {
--     poolSize=4,
--     timeout=secondsToNominalDiffTime 30,
--     Configs.connectionSettings=connectionSettings
-- }

-- connectionSettings = ConnectionSettings {
--     dbhost="localhost", 
--     dbport=5124,
--     dbuser="postgres",
--     dbpassword="",
--     database=""
-- }

-- walletConfig = WalletConfig poolSettings blockchainConfig nodeConfig

-- blockchainConfig = BlockchainConfig 4 (ForkMaxDiff 3) (Genesis "Złoty")

data CommandOptions = CommandOptions {
    configFilepath :: FilePath
}

parseCommand :: ParserInfo CommandOptions
parseCommand = info parseCommand
    (fullDesc <> progDesc "Run a wallet node.")
    where 
        parseCommand = CommandOptions
            <$> strOption
                (  long "config"
                <> metavar "TARGET"
                <> help "Filepath for the config file" )


main = do
    CommandOptions configFilepath <- execParser parseCommand
    eitherConfig <- decodeFileEither configFilepath `onException` (putStrLn "main: Quits." >> exitFailure)
    case eitherConfig of
        Left error -> do
            putStrLn $ prettyPrintParseException error
            print "Unable to read config file. Quits."
            exitFailure
        Right config -> do
            runWallet config
-- main = encodeFile "wallet_config.yaml" walletConfig