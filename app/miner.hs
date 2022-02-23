
import FullNode (runFullNode)
import Network.Socket
import Control.Monad (void)
import Control.Concurrent.Async
import Options.Applicative
import Control.Exception (onException)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Exit (exitFailure)
import Node (LoggingMode(ToFile))

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

main = do
    CommandOptions configFilepath <- execParser parseCommand
    eitherConfig <- decodeFileEither configFilepath `onException` (putStrLn "main: Quits." >> exitFailure)
    case eitherConfig of
        Left error -> do
            putStrLn $ prettyPrintParseException error
            print "Unable to read config file. Quits."
            exitFailure
        Right config -> do
            runFullNode config