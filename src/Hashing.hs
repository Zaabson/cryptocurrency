{-# LANGUAGE DeriveGeneric, RankNTypes #-}

module Hashing where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.Aeson ( encode, FromJSON(parseJSON), ToJSON(toJSON) )
import GHC.Generics
import Crypto.Util (bs2i, i2bs_unsized)
import Control.DeepSeq

-- TODO: target hash based on average mining speed 
-- targetHash :: RawHash
-- targetHash = RawHash $ B.pack $ replicate 10 0 ++ replicate 22 255 

newtype TargetHash = TargetHash RawHash

-- Gives TargetHash for a number in range.
-- difficulty ∈ [0, 32*4] where 128 is impossible and 0 is trivial difficulty.
difficultyToTargetHash :: Int -> TargetHash
difficultyToTargetHash n = TargetHash . RawHash . B.pack $ replicate (n `div` 4)  0 ++ [fromIntegral $ (n `mod` 4) * 64] ++ replicate (32-(n `div` 4) - 1) 255 

-- NOTE : it's better to keep strict Bytestrings as
--        fromStrict is O(1) and toStrict is O(n)

newtype HashOf a = Hash {getHash :: B.ByteString} deriving (Generic)

-- instance NFData (forall a . HashOf a)
instance NFData (HashOf a)

instance Show (HashOf a) where
    show (Hash bytes) = "Hash " ++ UTF8.toString (B.take 4 bytes) ++ "..."

instance Eq (HashOf a) where 
    (Hash b) == (Hash c) = b == c
instance Ord (HashOf a) where
    compare (Hash b) (Hash s) = compare b s


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
newtype RawHash = RawHash {rawHash :: B.ByteString} deriving (Eq, Ord, Generic)

instance NFData RawHash

instance Show RawHash where
    show (RawHash bytes) = "RawHash " ++ UTF8.toString (B.take 4 bytes) ++ "..."

instance ToJSON RawHash where
    toJSON (RawHash b) = toJSON $ bs2i b

instance FromJSON RawHash where
    parseJSON v = RawHash . i2bs_unsized <$> parseJSON v

shashBytes :: B.ByteString -> B.ByteString
shashBytes = SHA256.hash

-- forgets type of a previously hashed thing  
toRawHash :: HashOf a -> RawHash
toRawHash (Hash bytes) = RawHash bytes