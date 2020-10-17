{-# LANGUAGE ScopedTypeVariables #-}

import Merkle
import BlockType(Transaction(..))
import Test.QuickCheck
import Control.Parallel (pseq)

prop_leastPowerOf2 n = n > 0 ==> 2 ^ i >= n && 2 ^ (i - 1) <= n
    where i = leastPowerOf2 n

-- merkleHash on lists length 2^n
-- test passed if no errors occur
prop_powerOf2DoesntBreak = do 
    n :: Int <- choose (1, 12)
    let res = merkleHash $ replicate (2 ^ n) (Transaction [] [])
    return $ res `pseq` True

prop_notPowerOf2Break = expectFailure $ do 
    n :: Int <- choose (1, 12)
    let res = merkleHash $ replicate (2 ^ n * 3) (Transaction [] [])
    return $ res `pseq` True


main = do
    quickCheck prop_powerOf2DoesntBreak
    quickCheck prop_notPowerOf2Break
    quickCheck prop_leastPowerOf2