module Merkle where

import Control.Arrow ((>>>))
import qualified Data.ByteString as B
import BlockType (Transaction)
import Hashing (RawHash(..), HashOf(..), shash256, shashBytes)

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
          pairAndHash = splitInPairs >>> map (uncurry B.append >>> shashBytes)
          untilOneLeft _ [x] = [x]
          untilOneLeft _ []  = error "You sure list length is 2^n?"
          untilOneLeft f xs  = untilOneLeft f $ f xs