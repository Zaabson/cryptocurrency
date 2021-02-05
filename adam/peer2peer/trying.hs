{-# LANGUAGE ExplicitForAll #-}
import Network.Socket
import Control.Concurrent
import Control.Monad.State (StateT, get, evalStateT, State, state)

{- functions of type s -> IO ()
where s is working app -}

-- !! need seperate in and out addresses

-- ! use UDP as no need for statefull connections

-- need for persistent state (miner node will collect transactions)



type Address = (String, Int)

data Config = Config {
    startAddresses :: [Address],
    txsNumber      :: Integer
}
