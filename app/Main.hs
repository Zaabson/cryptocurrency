module Main where

-- import App (runNode, Config(..), LoggingMode(..))
-- import App
-- import App (runNode, Config(..), LoggingMode(..))
-- import App
import FullNode (runFullNode, withAppDo)
import BlockChain (FixedBlocks(..))
import Network.Socket
import Control.Monad (void)
import Data.Aeson (encodeFile, eitherDecodeFileStrict)
import Control.Concurrent.Async
import Options.Applicative
import Control.Exception (finally, onException)
import Node (RunningApp(RunningApp))
import GHC.IO.Handle (hClose)
import GHC.IO.FD (stdout)

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

-- TODO: Bracket
main = do
    CommandOptions configFilepath <- execParser parseCommand
    eitherConfig <- eitherDecodeFileStrict configFilepath `onException` putStrLn "main: Quits."
    case eitherConfig of
        Left error -> do
            print error
            print "Unable to read config file. Quits."
        Right config -> do
            withAppDo config (const $ return ())
--             -- encodeFile "app/fixed_blocks.json" (FixedBlocks [])