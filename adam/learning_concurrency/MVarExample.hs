{-# LANGUAGE LambdaCase #-}

import Control.Monad (join)
import Control.Concurrent
import Control.Exception (IOException, try)
import qualified Data.Map as M


data ThreadStatus = Finished 
                  | Running
                  | Threw IOException
                deriving (Eq, Show)

newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)

newManager :: IO ThreadManager
newManager = undefined

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mvar) io = 
    modifyMVar mvar $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            result <- try io
            putMVar state (either Threw (const Finished) result)
        return (M.insert tid state m, tid) 


getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mvar) tid =
    modifyMVar mvar $ \m ->
        case M.lookup tid m of
            Nothing -> return (m, Nothing)
            Just mtid -> tryTakeMVar mtid >>= \case
                                Nothing -> return (m, Just Running)
                                Just sth -> return (M.delete tid m, Just sth)

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
    maybeDone <- modifyMVar mgr $ \m ->
        return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
          (Nothing, _) -> (m, Nothing)
          (done, m') -> (m', done)
    case maybeDone of
        Nothing -> return Nothing
        Just st -> Just `fmap` takeMVar st

waitFor2 :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor2 (Mgr mgr) tid =
    join . modifyMVar mgr $ \m ->
        return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
                (Nothing, _) -> (m, return Nothing)
                (Just mstat, m') -> (m', Just `fmap` takeMVar mstat)

waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)

communicate = do
    mvar <- newEmptyMVar
    forkIO $ do
        message <- takeMVar mvar
        putStrLn $ "Received " ++ show message
    putStrLn "Here I am"
    putMVar mvar "Pstt, you pretty"

main = undefined