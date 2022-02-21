{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE DefaultSignatures #-}

module InMemory where

import Control.Concurrent.STM (TVar, STM, writeTVar, atomically, readTVar, modifyTVar')
import qualified Control.Concurrent.AdvSTM as ASTM
import Control.Concurrent.AdvSTM (AdvSTM)
import qualified Control.Concurrent.AdvSTM.TVar as ASTM

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


instance MonadAtomic AdvSTM where
    runAtomically = ASTM.atomically

instance InMemory (ASTM.TVar a) AdvSTM a where
    readMemory = ASTM.readTVar
    writeMemory = ASTM.writeTVar
    modifyMemory tvar f = ASTM.readTVar tvar >>= ASTM.writeTVar tvar . f
