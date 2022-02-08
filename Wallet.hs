module Wallet where     

import Database.LMDB.Simple

data Status
    = Validated 
    | Waiting
    | Discarded

data Coin = Coin OwnedUTXO Status

data DB mode k v = 
    DB {getEnvironment :: Environment mode,
        getDB :: Database k v} 

-- addCoin :: BlockReference -> OwnedUTXO -> ReaderT (DB ReadWrite BlockReference [OwnedUTXO]) IO ()
-- addCoin blockref coin = do
--     env <- asks getEnvironment
--     env <- asks getDB
--     -- TODO: maybe also early status update 
--     readWriteTransaction env $
--         adjust (coin : ) blockref db

data UpdateStatus
    = Fixed { getBlockRef :: BlockReference }
    | Discard { getBockRef :: BlockReference}


doTransaction :: (Mode mode, SubMode submode mode) => Transaction submode a  -> ReaderT (DB mode k v) IO a
doTransaction tx = do 
    env <- asks getEnvironment
    env <- asks getDB
    transaction env (tx db)
        

-- updateStatuses :: UpdateStatus -> ReaderT (DB ReadWrite BlockReference [OwnedUTXO]) IO ()
updateStatuses :: Database BlockReference [Coin] -> UpdateStatus -> Transaction ReadWrite ()
updateStatuses update = adjust (map f) (getBlockRef update) db
    where 
        f (Fixed blockref) (Coin utxo _) = Coin utxo Validated
        f (Discard blockref) (Coin utxo _) = Coin utxo Discarded


data WalletConfig

runWallet :: WalletConfig -> IO ()
runWallet config = do
    env  <- openEnvironment (getDBFilepath config) defaultLimits
    fixeddb <- readWriteTransaction env $ getDatabase (Just "fixed_db") :: IO (Database Integer BlockHeader)
    coinsdb <- readWriteTransaction env $ getDatabase (Just "coins_db") :: IO (Database BlockReference [OwnedUTXO])
    _