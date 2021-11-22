{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE DefaultSignatures #-}

module InMemory where

import Control.Concurrent.STM (TVar, STM, writeTVar, atomically, readTVar, modifyTVar')

class HasLogging env where
    logger :: env -> String -> IO ()


class InMemoryRead tvar a where
    readMemoryIO :: tvar -> IO a 

class Monad m => MonadAtomic m where
    runAtomically :: m b -> IO b

class MonadAtomic m => InMemory tvar m a | tvar -> m where
    readMemory :: tvar -> m a
    writeMemory :: tvar -> a -> m ()
    modifyMemory :: tvar -> (a -> a) -> m ()
    modifyMemory tvar f = readMemory tvar >>= (writeMemory tvar . f)
    
    -- include in class to be able to overwrite with more efficient
    modifyMemoryIO :: tvar -> (a -> a) -> IO ()
    modifyMemoryIO tvar f = runAtomically $ modifyMemory tvar f
    
instance InMemory tvar m a => InMemoryRead tvar a where
    readMemoryIO = runAtomically . readMemory

instance MonadAtomic STM where
    runAtomically = atomically

instance InMemory (TVar a) STM a where
    readMemory = readTVar
    writeMemory = writeTVar
    modifyMemory = modifyTVar'

