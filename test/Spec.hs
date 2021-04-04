{-# LANGUAGE ScopedTypeVariables #-}

import Merkle
import BlockType(Transaction(..), Coinbase,Genesis (Genesis))
import BlockChainTest
import Test.QuickCheck
import Control.Parallel (pseq)
import ArbitraryBlock
import BlocksValidationTest
import BlockCreation
import Text.Pretty.Simple (pPrint)
import qualified Codec.Crypto.RSA as RSA
import qualified Data.ByteString.Lazy as LazyB
import Hashing (HashOf(getHash), shash256)

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

-- Tests fail because transactions with no outputs use up inputs in arbitraryBlockchain. But that's good no?

-- The other way around: validateBlockTransactions only inserts new outputs, but doesn't 
-- remove the ones that were referenced in inputs. 

main = do
    -- sample' arbitraryBlockchain >>= mapM (\(_, blocks, genesis) -> pPrint genesis >> pPrint blocks)
    -- sample' arbitraryBlockchain
    quickCheckWith (stdArgs {maxSize = 10}) prop_reverseToZipper
    quickCheckWith (stdArgs {maxSize = 10}) prop_UTXOPoolCorrect