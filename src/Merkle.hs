module Merkle where

import Control.Arrow ((>>>))
import qualified Data.ByteString as B
import BlockType (Transaction, Coinbase)
import Hashing (RawHash(..), HashOf(..), shash256, shashBytes)
import Data.Function ((&))

-- Returns least number i>=1 such that 2^i is greater or equal to argument.
-- leastPowerOf2 :: Int -> Int
-- leastPowerOf2 n = loop 1
--     where loop i = if 2^i >= n then i else loop $ i + 1

-- pairs elements in a list, pair by pair, duplicates the last element if the list is odd length 
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = [(x, x)]
pairs (x:y:xs) = (x, y) : pairs xs 

-- calculates a merkle root hash of a list by first hashing a transactions in a list
-- and then pairing neighbouring hashes and combining them into single hash iteratively - until there's a single hash, root hash
-- coinbase transaction is appended at the beginning
merkleHash :: Coinbase -> [Transaction] -> RawHash
merkleHash coinbase = map (shash256 >>> getHash) >>> (
    (coinbase & shash256 & getHash) :) >>> untilOneLeft pairAndHash >>> head >>> RawHash
    where pairAndHash :: [B.ByteString] -> [B.ByteString]
          pairAndHash = pairs >>> map (uncurry B.append >>> shashBytes)
          untilOneLeft _ [x] = [x]
          untilOneLeft f xs  = untilOneLeft f $ f xs