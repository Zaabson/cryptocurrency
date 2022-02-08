{-# LANGUAGE OverloadedStrings  #-}
module Wallet.Wallet where
import BlockType (TXID, BlockReference, Transaction, BlockHeader (BlockHeader))
import Hashing (HashOf(Hash), RawHash (RawHash))
import Hasql.Statement (Statement (Statement))
import Data.Vector (Vector)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Data.Aeson (FromJSON, decodeStrict', eitherDecodeStrict', ToJSON, encode, toJSON)
import Data.Bifunctor (first)
import Data.Text (pack)
import Data.Functor.Contravariant (contramap)
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Contravariant.Extras (contrazip2)

-- Decision: Let's store transactions rather than utxos. We lose less information and its trivial to get utxos from tx.

data Status
    = Validated -- block of td already in fixed  
    | Waiting   -- still not in fixed 
    | Discarded -- optional usage for when we observe tx being thrown from lively.
    -- ^ another option is to use versioning with blockheight 

data UpdateStatus
    = Fixed { getBlockRef :: BlockReference }
    | Discard { getBockRef :: BlockReference}


-- 
data StoredTransaction = StoredTransaction {
    txid :: TXID,
    txBlockId :: BlockReference,
    txData :: Transaction,
    txStatus :: Status
}

data StoredBlockHeader = StoredBlockHeader {
    blockid :: BlockReference,
    headerData :: BlockHeader
}

jsonb2aeson :: FromJSON a => D.Value a
jsonb2aeson = D.jsonbBytes (first pack . eitherDecodeStrict')

aeson2jsonb :: ToJSON a => E.Value a
aeson2jsonb = contramap toJSON E.jsonb

rowHash :: D.Row (HashOf a)
rowHash = coerceHash <$> D.column (D.nonNullable D.bytea)
    where
        coerceHash :: ByteString -> HashOf a
        coerceHash = coerce

encodeHash :: E.Params BlockReference
encodeHash = coerce <$> E.param . E.nonNullable $ E.bytea

txDecoder :: D.Row StoredTransaction
txDecoder = StoredTransaction
    <$> rowHash
    <*> rowHash
    <*> D.column (D.nonNullable jsonb2aeson)
    <*> D.column (D.nonNullable (D.enum str2status))

    where 
        str2status "validated" = Just Validated
        str2status "waiting" = Just Waiting
        str2status "discarded" = Just Discarded
        str2status _ = Nothing

encodeStatus :: E.Params Status 
encodeStatus = E.param . E.nonNullable $ E.enum status2str
    where
        status2str Validated = "validated"
        status2str Waiting   = "waiting"
        status2str Discarded = "discarded"

        
selectTxByStatus :: Statement Status (Vector StoredTransaction)
selectTxByStatus = Statement sql encodeStatus (D.rowVector txDecoder) True
    where
        sql = "select (txId, txBlockId, txData, txStatus) from transaction where txStatus = $1"


selectTxByBlock :: Statement BlockReference (Vector StoredTransaction)
selectTxByBlock = Statement sql encodeHash (D.rowVector txDecoder) True
    where 
        sql = "select (txId, txBlockId, txData, txStatus) from transaction where txBlockId = $1"


-- Update txs statuses for transactions from block
updateTx :: Statement (BlockReference, Status) Int64
updateTx = Statement sql (contrazip2 encodeHash encodeStatus) D.rowsAffected True
    where 
        sql = "update transaction set txStatus = $2 where txBlockId = $1"

