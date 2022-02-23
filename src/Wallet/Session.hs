module Wallet.Session where 
    
-- import Hasql.Session (Session, statement)
-- import Data.Vector (Vector)

-- import qualified Wallet.Statement as Statement
-- import BlockType (BlockReference, TXID)
-- import Wallet.Type (Status, StoredTransaction)

-- selectTxByStatus :: Status -> Session (Vector StoredTransaction)
-- selectTxByStatus status = statement status Statement.selectTxByStatus


-- selectTxByBlock :: BlockReference -> Session (Vector StoredTransaction)
-- selectTxByBlock blockRef = statement blockRef Statement.selectTxByBlock 

-- selectTxIdByBlock :: BlockReference  -> Session (Vector TXID)
-- selectTxIdByBlock blockRef = statement blockRef Statement.selectTxIdByBlock 

-- updateTxStatus :: Status -> TXID -> Session ()
-- updateTxStatus status txid = statement (status, txid) Statement.updateTxStatus

-- updateTxStatusMany :: Status -> Vector TXID -> Session ()
-- updateTxStatusMany status txids = statement (status, txids) Statement.updateTxStatusMany 
