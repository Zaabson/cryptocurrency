{-# LANGUAGE DeriveGeneric #-}

module Hashing where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import Data.Aeson ( encode, FromJSON(parseJSON), ToJSON(toJSON) )
import GHC.Generics
import Crypto.Util (bs2i, i2bs_unsized)

-- NOTE : it's better to keep strict Bytestrings as
--        fromStrict is O(1) and toStrict is O(n)

newtype HashOf a = Hash {getHash :: B.ByteString} deriving (Show, Generic)

-- JSON serialization achieved by converting hash ByteString to Integer
instance ToJSON (HashOf a) where 
    toJSON (Hash b) = toJSON $ bs2i b
instance FromJSON (HashOf a) where
    parseJSON v = Hash . i2bs_unsized <$> parseJSON v

-- serializes to ByteString using JSON serialization, calculates sha256 hash
-- Hashable == ToJSON
shash256 :: ToJSON a => a -> HashOf a
shash256 = Hash . SHA256.hashlazy . encode

-- type to be used instead of HashOf when the information about what was hashed can be lost.
newtype RawHash = RawHash {rawHash :: B.ByteString} deriving (Eq, Show)

instance ToJSON RawHash where
    toJSON (RawHash b) = toJSON $ bs2i b

instance FromJSON RawHash where
    parseJSON v = RawHash . i2bs_unsized <$> parseJSON v

shashBytes :: B.ByteString -> B.ByteString
shashBytes = SHA256.hash