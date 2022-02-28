{-# LANGUAGE DeriveGeneric, RankNTypes #-}

module Hashing where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON) )
import qualified Data.Aeson as Aeson
import GHC.Generics ( Generic )
import Crypto.Util (bs2i, i2bs_unsized)
import Control.DeepSeq ( NFData )
-- import qualified Data.Aeson.Key as Aeson

-- TODO: target hash based on average mining speed 
-- targetHash :: RawHash
-- targetHash = RawHash $ B.pack $ replicate 10 0 ++ replicate 22 255 

newtype TargetHash = TargetHash RawHash

-- Gives TargetHash for a number in range.
-- difficulty ∈ [0, 32] where 0 is impossible and 32 is trivial difficulty, changing linearly.
difficultyToTargetHash :: Int -> TargetHash
-- difficultyToTargetHash n = TargetHash . RawHash . B.pack $ (replicate n 0 <> replicate (32 - n) 255)
difficultyToTargetHash n = TargetHash . RawHash . int2bytes $ min (((maxN + 1) `div` 32) * k) maxN
    where k = max 0 (min maxN (toInteger n))

maxN :: Integer
maxN = 256^32-1

int2bytes :: Integer -> B.ByteString
int2bytes n = B.pack $ reverse $ take 32 (unfold n)
    where
        unfold n =
            let (d, m) = n `divMod` 256
                in fromInteger m : unfold d

bytes2int :: B.ByteString  -> Integer
bytes2int = B.foldl' (\n c -> n*256 + toInteger c) 0

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
shash256 = Hash . SHA256.hashlazy . Aeson.encode

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