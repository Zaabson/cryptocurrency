module Wallet.Session where

import qualified Hasql.Transaction as DBTransaction
import BlockType (BlockReference, TXID, BlockHeader)
import Wallet.Type (Status (Validated), StoredTransaction (StoredTransaction))
import qualified Hasql.Transaction as Transaction
-- import qualified Wallet.Statement (selectTxIdByBlock)
import qualified Data.Vector as V
import qualified Wallet.Statement as Statement
import Hashing (shash256)
import qualified Hasql.Session as Session
import Data.Int (Int64, Int32)
import qualified Codec.Crypto.RSA as RSA
import qualified Data.Aeson as Aeson

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

insertTransaction :: StoredTransaction -> Session.Session ()
insertTransaction (StoredTransaction txid blockref etx status) = 
    Session.statement 
        (txid, blockref, either Aeson.toJSON Aeson.toJSON etx, status, either (const False) (const True) etx)
        Statement.insertTransaction   

insertOwnedKeys :: TXID -> Int32 -> RSA.PublicKey -> RSA.PrivateKey ->  Session.Session ()
insertOwnedKeys txid vout pub priv = Session.statement (txid, vout, pub, priv) Statement.insertOwnedKeys 

selectStatus :: TXID -> Session.Session Status
selectStatus txid = Session.statement txid Statement.selectStatus 