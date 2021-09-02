module Main where

-- import App (runNode, Config(..), LoggingMode(..))
import App
import BlockChain (FixedBlocks(..))
import Network.Socket
import Data.Aeson (encodeFile, eitherDecodeFileStrict)
import Control.Concurrent.Async
import Options.Applicative

main = do
    CommandOptions configFilepath <- execParser parseCommand
    eitherConfig <- eitherDecodeFileStrict configFilepath
    case eitherConfig of
        Left error -> do
            print error
            print "Unable to read config file. Quits."
        Right config -> withAppDo config (\_ _ -> return ())
--             -- encodeFile "app/fixed_blocks.json" (FixedBlocks [])
