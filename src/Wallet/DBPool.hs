{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Wallet.DBPool where

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Data.ByteString (ByteString)
import Data.Binary (Word16)
import Data.Time (NominalDiffTime)
import Hasql.Session (Session)
import Control.Exception (bracket)
import Hasql.Connection (settings)
import Control.Monad ((>=>))
import System.Exit (exitFailure)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data ConnectionSettings = ConnectionSettings {
    host :: ByteString,
    port :: Word16,
    user :: ByteString,
    password :: ByteString,
    database :: ByteString
} deriving (Generic)

-- instance ToJSON ConnectionSettings
-- instance FromJSON ConnectionSettings



data PoolSettings = PoolSettings {
    poolSize :: Int,
    timeout :: NominalDiffTime,
    connectionSettings :: ConnectionSettings
}


-- withPool :: PoolSettings -> ((Session a -> IO (Either Pool.UsageError a)) -> IO b) -> IO b
-- withPool PoolSettings{connectionSettings=ConnectionSettings{..}, ..} f = 
--     bracket 
--         (Pool.acquire (poolSize, timeout, settings host port user password database))
--         Pool.release
--         (f . Pool.use)

acquire :: PoolSettings -> IO Pool 
acquire PoolSettings{connectionSettings=ConnectionSettings{..}, ..} = 
    Pool.acquire (poolSize, timeout, settings host port user password database)


-- withPool :: HasDB appState => PoolSettings -> (() -> appState -> IO b) -> IO b

onErrorLogAndQuit :: (String -> IO ()) -> (Session a -> IO (Either Pool.UsageError a)) -> (Session a -> IO a) 
onErrorLogAndQuit log f = f >=> \case 
   Left  e -> log (show e) >> exitFailure
   Right a -> return a

-- Question: Can I recover from errors?



class HasDB appState where 
    executeDB :: appState -> Session a -> IO a
