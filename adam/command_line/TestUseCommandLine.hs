{-# LANGUAGE LambdaCase #-}

module TestUseCommandLine (testMain) where
import qualified Data.Map as Map
import CommandLine
import Control.Monad.Trans (liftIO)

type CommandArgValidator = CommandWithArgs -> Bool
type ExecutableCommand s = CommandWithArgs -> StateIO s

myCommands :: Map.Map String (CommandArgValidator, ExecutableCommand ()) 
myCommands = Map.insert "print" (printValidator, printCommand) Map.empty

printValidator :: CommandArgValidator
printValidator comm = null (options comm) &&
                      (not . null $ arguments comm) &&
                      (commandName comm == "print")

printCommand :: ExecutableCommand ()
printCommand CommandWithArgs{arguments=args} = liftIO (putStrLn $ concat args)

findCommand :: FindCommand ()
findCommand comm = Map.lookup (commandName comm) myCommands >>= \case 
        (validator, executable) -> if validator comm
                                   then Just $ executable comm
                                   else Nothing

testMain :: IO ()
testMain = runCLI findCommand ()