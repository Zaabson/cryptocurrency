{-# LANGUAGE ScopedTypeVariables #-}

import Merkle
import BlockType(Transaction(..), Coinbase)
import BlockChainTest
import Test.QuickCheck
import Control.Parallel (pseq)

-- prop_leastPowerOf2 n = n > 0 ==> 2 ^ i >= n && 2 ^ (i - 1) <= n
--     where i = leastPowerOf2 n


-- these 2 tests are not very smart as now merkleHash is total:

-- merkleHash on lists length 2^n
-- test passed if no errors occur
-- prop_ merkleHash = do 
--     n :: Int <- choose (1, 12)
--     let res = merkleHash (Coinbase 420 []) $ replicate (2 ^ n) (Transaction [] [])
--     return $ res `pseq` True  -- i don't remember what point did it have? is it alright?

-- prop_notPowerOf2DoesntBreak = do 
--     n :: Int <- choose (1, 12)
--     let res = merkleHash $ replicate (2 ^ n * 3) (Transaction [] [])
--     return $ res `pseq` True


main = do
    quickCheckWith (stdArgs {maxSize = 10}) prop_reverseToZipper