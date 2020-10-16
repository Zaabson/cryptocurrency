module Merkle where

import Control.Arrow ((>>>))
import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA256 as SHA256
-- import BlockType (HashOf(rawHash), Transaction, shash256)
import BlockType (HashOf(..), Transaction, shash256)

-- type to be used instead of HashOf when the information about what was hashed can be lost.
newtype RawHash = RawHash B.ByteString

-- Returns least number i>=1 such that 2^i is greater or equal to argument.
leastPowerOf2 :: Int -> Int
leastPowerOf2 n = loop 1
    where loop i = if 2^i >= n then i else loop $ i + 1

-- Assumes list to be even length!
splitInPairs :: [a] -> [(a, a)]
splitInPairs [] = []
splitInPairs (x:y:xs) = (x, y) : splitInPairs xs 

merkleHash :: [Transaction] -> RawHash
merkleHash = map (shash256 >>> getHash) >>> untilOneLeft pairAndHash >>> head >>> RawHash
    where pairAndHash :: [B.ByteString] -> [B.ByteString]
          pairAndHash = splitInPairs >>> map (uncurry B.append >>> SHA256.hash)
          untilOneLeft _ [x] = [x]
          untilOneLeft _ []  = error "You sure list length is 2^n?"
          untilOneLeft f xs  = untilOneLeft f $ f xs