{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, GeneralizedNewtypeDeriving, RecordWildCards #-}

module BlockType where

import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LazyB
import Data.Bits (shiftL, shiftR)
import qualified Data.Binary as Binary
import Data.Aeson ( encode, FromJSON(parseJSON), ToJSON(toJSON) )
import Data.Time.Clock (UTCTime)
import Crypto.Util (bs2i, i2bs_unsized)
import qualified Codec.Crypto.RSA as RSA -- Using RSA instead of elliptic curves because the library was better documented
import Hashing (HashOf, RawHash)

newtype Cent = Cent Integer deriving (Show, Generic, Num, Ord, Eq)  -- currency unit

instance FromJSON Cent
instance ToJSON Cent

type TXID = HashOf (Either Coinbase Transaction)
type PublicAddress = HashOf RSA.PublicKey

newtype Signature = Signature B.ByteString deriving (Show, Generic)

-- Serialization achieved by converting ByteString to Integer
instance ToJSON Signature where 
    toJSON (Signature b) = toJSON $ bs2i b
instance FromJSON Signature where 
    parseJSON v = Signature . i2bs_unsized <$> parseJSON v


-- Serialization achieved by converting PublicKey to ByteString (using Binary) and then to Integer
instance ToJSON RSA.PublicKey where 
    toJSON = toJSON . lazy_bs2i . Binary.encode

instance FromJSON RSA.PublicKey where
    parseJSON v = Binary.decode . lazy_i2bs_unsized <$> parseJSON v

-- from "crypto-api" Crypto.Util
lazy_bs2i :: LazyB.ByteString -> Integer
lazy_bs2i = LazyB.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0
lazy_i2bs_unsized :: Integer -> LazyB.ByteString
lazy_i2bs_unsized 0 = LazyB.singleton 0
lazy_i2bs_unsized i = LazyB.reverse $ LazyB.unfoldr (\i' -> if i' <= 0 then Nothing else Just (fromIntegral i', i' `shiftR` 8)) i

data Transaction = Transaction {
        inputs :: [Input],
        outputs :: [Output]
    } deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON Transaction

data Coinbase = Coinbase {
    blockHeight :: Integer, -- included so that coinbase transactions to the same address hash differently
    coinbaseOutputs :: [Output]
    } deriving (Show, Generic)

instance FromJSON Coinbase
instance ToJSON Coinbase

data Output = Output {
    outputDenomination :: Cent,
    ownerPublicAddress :: PublicAddress -- public's key hash
    } deriving (Show, Generic)

instance FromJSON Output
instance ToJSON Output

data Input = Input {
    signature :: Signature,
    signerPublicKey :: RSA.PublicKey,
    utxoReference :: TXID, -- reference to UTXO
    vout :: Integer -- output index in a transaction outputs, reference to UTXO
    } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

newtype Genesis = Genesis String deriving (Show, ToJSON, FromJSON)
-- instance FromJSON Genesis
-- instance ToJSON Input

type BlockReference = HashOf (Either Genesis BlockHeader)

data BlockHeader = BlockHeader {
        nonce :: Integer,
        previousHash :: BlockReference,
        timestamp :: UTCTime,
        rootHash :: RawHash
    } deriving (Show, Generic)

instance FromJSON BlockHeader
instance ToJSON BlockHeader

data Block = Block {
    blockHeader :: BlockHeader,
    coinbaseTransaction :: Coinbase,
    transactions :: [Transaction]
    } deriving (Show, Generic)

instance FromJSON Block
instance ToJSON Block

-- TODO: Change these to lenses (don't think it makes sense):

blockNonce :: Block -> Integer
blockNonce = nonce . blockHeader

blockPreviousHash :: Block -> HashOf (Either Genesis BlockHeader)
blockPreviousHash = previousHash . blockHeader

blockTimestamp :: Block -> UTCTime
blockTimestamp = timestamp . blockHeader

blockRootHash :: Block -> RawHash
blockRootHash = rootHash . blockHeader

blockBlockHeight :: Block -> Integer
blockBlockHeight Block{coinbaseTransaction=c, ..} = blockHeight c