{-# LANGUAGE ScopedTypeVariables #-}

import Merkle
import BlockType(Transaction(..), Coinbase,Genesis (Genesis))
import BlockChainTest
import Server (int64ToByteString, byteStringToInt64)
import Test.QuickCheck
import Control.Parallel (pseq)
import ArbitraryBlock
import BlocksValidationTest
import BlockCreation
import Text.Pretty.Simple (pPrint)
import qualified Codec.Crypto.RSA as RSA
import qualified Data.ByteString.Lazy as B
import Hashing (HashOf(getHash), shash256)
import Data.Int

prop_bytestringToInt64 :: Int64 -> Bool
prop_bytestringToInt64 x = x == byteStringToInt64 (int64ToByteString x)


main = do
    -- sample' arbitraryBlockchain >>= mapM (\(_, blocks, genesis) -> pPrint genesis >> pPrint blocks)
    -- sample' arbitraryBlockchain
    quickCheck prop_bytestringToInt64
    quickCheckWith (stdArgs {maxSize = 10}) prop_reverseToZipper
    quickCheckWith (stdArgs {maxSize = 10}) prop_UTXOPoolCorrect