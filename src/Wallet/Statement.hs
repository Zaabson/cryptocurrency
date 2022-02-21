{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts #-}
module Wallet.Statement where

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
import Data.Coerce (coerce, Coercible)
import Unsafe.Coerce (unsafeCoerce)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Contravariant.Extras (contrazip2)
import Data.Foldable (foldl')
import Wallet.Type (StoredTransaction (StoredTransaction), Status (Validated, Waiting, Discarded))


jsonb2aeson :: FromJSON a => D.Value a
jsonb2aeson = D.jsonbBytes (first pack . eitherDecodeStrict')

aeson2jsonb :: ToJSON a => E.Value a
aeson2jsonb = contramap toJSON E.jsonb

rowHash :: D.Row (HashOf a)
rowHash = coerceHash <$> D.column (D.nonNullable D.bytea)
    where
        coerceHash :: ByteString -> HashOf a
        coerceHash = coerce

-- encodeHash :: E.Params BlockReference
encodeHash :: Coercible b ByteString =>  E.Params b
encodeHash = contramap coerce . E.param . E.nonNullable $ E.bytea

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

selectTxIdByBlock :: Statement BlockReference (Vector TXID)
selectTxIdByBlock = Statement sql encodeHash (D.rowVector rowHash) True
    where 
        sql = "select txId from transaction where txBlockId = $1"

updateTxStatus :: Statement (Status, TXID) ()   
updateTxStatus = Statement sql (contrazip2 encodeStatus encodeHash) D.noResult True
    where 
        sql = "update transaction set txStatus = $2 where txBlockId = $1"


-- Update txs statuses for transactions from block
-- updateTx :: Statement (BlockReference, Status) Int64
-- updateTx = Statement sql (contrazip2 encodeHash encodeStatus) D.rowsAffected True
--     where 
--         sql = "update transaction set txStatus = $2 where txBlockId = $1"

updateTxStatusMany :: Statement (Status, Vector TXID) ()
updateTxStatusMany = Statement sql (contrazip2 encodeStatus (vector $ contramap coerce E.bytea)) D.noResult True
    where
        sql = "update transaction set txStatus = $1 from unnest($2) as t(num) where t.num = txId"
        vector =
            E.param .
            E.nonNullable .
            E.array .
            E.dimension foldl' .
            E.element .
            E.nonNullable

addFixedBlockHeader :: Statement (BlockReference , BlockHeader) ()
addFixedBlockHeader = Statement sql e D.noResult True
    where 
        sql = "insert into fixedHeader values ($1, $2)"
        e = contrazip2 encodeHash (E.param . E.nonNullable $ aeson2jsonb)

selectFixedCount :: Statement () Int64
selectFixedCount = Statement sql E.noParams (D.singleRow . D.column . D.nonNullable $ D.int8) True 
    where
        sql = "select count(*) from fixedHeader"