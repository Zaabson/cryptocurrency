{-# LANGUAGE LambdaCase #-}
module Ghci_test where
import Control.Concurrent.Async
import System.IO (openFile, IOMode (ReadWriteMode), hClose, hPutStrLn, hFlush, stdout)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, replicateM, replicateM_)
import Control.Exception (bracket, finally)
import Node (LoggingMode(ToStdout), withLogging)
import Data.Maybe (Maybe(Nothing))
import BlockChain (Tree(Tree))
-- Test concludes that closing ghci doesn't close gracefully async actions running in interpreter. 

load = openFile "ghci_test_file" ReadWriteMode

quit file = do
    hPutStrLn file "Quitting."
    hClose file

action file = forever $ do
    threadDelay 1000000
    hPutStrLn file "counting..."

waiting = do
    putStrLn "Waited.."

bar = bracket load quit (\hdl -> bracket (async $ action hdl) (const waiting) wait) `finally` putStrLn "Bye!"

test1 = do
    withAsync (bracket
        load
        quit
        (\hdl -> bracket (async $ action hdl) (const waiting) wait))
        (const $ return 5)

foo = do
    bracket (async ((forever (threadDelay 3000000 >> print "Done!")) `finally` (print "Finaly" >> hFlush stdout)))
            cancel wait

bracketOrder = bracket (print "start") (const $ print "stop") (const $ bracket (print "begin") (const $ print "finish") (const $ threadDelay 100000))
-- ^ result : inner first

-- Test logger with multiple threads.
loggerThreadsTest = withLogging ToStdout $ \log -> do
    log "start"
    async (replicateM_ 7 $ printing log "A")
    threadDelay 400000
    async (forever $ printing log "B")
    log "waiting"
    -- awaitSignal Nothing
    forever (threadDelay 1000000)

    where
        printing log str = log str >> threadDelay 1000000

depth maximum [] = 0
depth maximum ts = 1 + maximum (map (\case Tree _ tss -> depth maximum tss) ts)

main = loggerThreadsTest