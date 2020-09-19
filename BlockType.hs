{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, GeneralizedNewtypeDeriving, RecordWildCards #-}

module BlockType where

import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LazyB
import Data.Bits (shiftL, shiftR)
import qualified Data.Binary as Binary
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Crypto.Util (bs2i, i2bs_unsized)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Codec.Crypto.RSA as RSA -- Using RSA because library is better documented that the one i found for elliptic curves

-- NOTE : it's better to keep strict Bytestrings as
--        fromStrict is O(1) and toStrict is O(n)

newtype HashOf a = Hash {rawHash :: B.ByteString} deriving (Show, Generic)

-- Serialization achieved by converting ByteString to Integer
instance ToJSON (HashOf a) where 
    toJSON (Hash b) = toJSON $ bs2i b
instance FromJSON (HashOf a) where
    parseJSON v = Hash . i2bs_unsized <$> parseJSON v


newtype Cent = Cent Integer deriving (Show, Generic, Num, Ord, Eq)  -- currency unit

instance FromJSON Cent
instance ToJSON Cent


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

type TXID = HashOf Transaction
type PublicAddress = HashOf RSA.PublicKey

-- serialize to ByteString using JSON serialization, 
-- calculate sha256 hash and convert to Integer for storing in JSON
shash256 :: ToJSON a => a -> HashOf a
shash256 = Hash . SHA256.hashlazy . encode

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
    vout :: Integer -- input index in a transaction inputs, reference to UTXO
    } deriving (Show, Generic)

instance FromJSON Input
instance ToJSON Input

data BlockHeader = BlockHeader {
        nonce :: Integer,
        previousHash :: HashOf BlockHeader,
        timestamp :: UTCTime,
        rootHash :: HashOf B.ByteString  -- !!! temporary   
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

blockNonce :: Block -> Integer
blockNonce = nonce . blockHeader

blockPreviousHash :: Block -> HashOf BlockHeader
blockPreviousHash = previousHash . blockHeader

blockTimestamp :: Block -> UTCTime
blockTimestamp = timestamp . blockHeader

blockRootHash :: Block -> HashOf B.ByteString
blockRootHash = rootHash . blockHeader

blockHeightFromCoinbase :: Block -> Integer
blockHeightFromCoinbase Block{coinbaseTransaction=c, ..} = blockHeight c