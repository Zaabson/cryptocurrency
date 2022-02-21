module Wallet.DBTransaction where

import qualified Hasql.Transaction as DBTransaction
import BlockType (BlockReference, TXID, BlockHeader)
import Wallet.Type (Status (Validated))
import qualified Hasql.Transaction as Transaction
-- import qualified Wallet.Statement (selectTxIdByBlock)
import qualified Data.Vector as V
import qualified Wallet.Statement as Statement
import Hashing (shash256)
import qualified Hasql.Session as Session
import Data.Int (Int64)

-- TODO: if Session unused, rename to session

updateTxStatusMany :: Status -> V.Vector TXID -> Transaction.Transaction ()
updateTxStatusMany status txids = Transaction.statement (status, txids) Statement.updateTxStatusMany 


-- Select transactions with given blockRefence, change statuses to status for the ones that predicate evaluates true.
updateStatusByBlock :: BlockReference -> (TXID -> Bool) -> Status -> DBTransaction.Transaction ()
updateStatusByBlock blockref pred status = do 
    txids <- Transaction.statement blockref Statement.selectTxIdByBlock
    updateTxStatusMany Validated (V.filter pred txids)

addFixedBlockHeader :: BlockHeader -> DBTransaction.Transaction ()
addFixedBlockHeader blockHeader = Transaction.statement (shash256 $ Right blockHeader, blockHeader) Statement.addFixedBlockHeader

selectFixedCount :: Session.Session Int64
selectFixedCount = Session.statement () Statement.selectFixedCount