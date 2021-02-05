{-# LANGUAGE RankNTypes #-}

module CommandLine (runCLI, FindCommand, CommandWithArgs(CommandWithArgs),
                    commandName, options, arguments, StateIO) where

import Control.Monad (when)
import System.Exit (exitSuccess)
import Control.Monad.State (StateT, get, evalStateT, State, state)
import Control.Monad.Trans (liftIO)
import System.IO (isEOF, hFlush, stdout, hSetBuffering, BufferMode (NoBuffering))
import Text.ParserCombinators.Parsec
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

-- To use module provide function of type FindCommand.
-- That will do what you want.
type StateIO s = StateT s IO ()
type FindCommand s = CommandWithArgs -> Maybe (StateIO s)
-- StateT s IO String
data CommandWithArgs = CommandWithArgs {
    commandName :: String,
    options :: [Char],
    arguments :: [String]
}

command :: GenParser Char st CommandWithArgs
command = do
    name <- manyTill letter (space <|> newline <|> tab)
    options <- endBy (char '-' *> letter) spaces
    args <- sepBy (many1 letter) spaces
    return $ CommandWithArgs name options args
-- word = many letter
-- option = 


parseCommand :: String -> Either String CommandWithArgs
parseCommand input = case parse command "Error with parsing the command" input of
    Left error -> Left $ show error
    Right comm -> Right comm


promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

-- action :: StateT s IO a

doCLI :: FindCommand s -> StateIO s
doCLI find = do
        liftIO $ hSetBuffering stdout NoBuffering
        mainLoop
    where mainLoop = do
            line <- liftIO $ promptLine ">"
            liftIO $ print $ length line
            case parseCommand line of
                Left error -> liftIO $ putStrLn error
                Right command -> Data.Maybe.fromMaybe (liftIO $ putStrLn "Command not found!") (find command)
            mainLoop

        -- done <- liftIO isEOF
        -- when False $ liftIO $ putStrLn "Exiting program." >> exitSuccess

runCLI :: FindCommand s -> s -> IO ()
runCLI find = evalStateT (doCLI find)