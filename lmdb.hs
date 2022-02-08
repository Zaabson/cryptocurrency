{-# LANGUAGE ScopedTypeVariables #-}
module Lmdb where

import Database.LMDB.Simple
import Control.Monad (forM_)

main = do
  env :: Environment ReadOnly <- openEnvironment "myenv" defaultLimits
  db <- readOnlyTransaction env $ getDatabase Nothing :: IO (Database String Int)

  -- transaction env $
  --   forM_ [("one",1),("two",2),("three",3)] $ \(k,v) -> put db k (Just v)

  print =<< readOnlyTransaction env (get db "two")   -- Just 2
  print =<< readOnlyTransaction env (get db "one")  -- Nothing