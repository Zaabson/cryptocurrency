module BlocksValidationTest where

import BlockType
import ArbitraryBlock
import Test.QuickCheck
import qualified Data.Map as Map
import BlockValidation (UTXOPool(..), validateBlockTransactions,UTXO (UTXO))
import Data.List (foldl')
import BlockCreation (OwnedUTXO(OwnedUTXO))


foldBlockchain :: [Block] -> Maybe UTXOPool
foldBlockchain = foldl' f (Just (UTXOPool Map.empty))
    where
        f (Just (UTXOPool utxos)) block =
            case validateBlockTransactions (UTXOPool utxos) block of
                (True, newUtxos) -> Just newUtxos
                (False, _ )      -> Nothing
        f Nothing _ = Nothing

-- Checks whether UTXOs accumulated by folding validateBlockTransactions on a list of blocks
-- are the same that the ones arbitraryBlockchain accumulates in the process of generating blocks. 
prop_UTXOPoolCorrect :: BlockchainWithState -> Bool
prop_UTXOPoolCorrect (BlockchainWithState utxos blocks genesis) =
    case foldBlockchain (reverse blocks) of
        Just (UTXOPool utxoPool) -> 
            -- all utxoPool contains all utxos and theyre the same size, then utxoPool == utxos
            all (\(OwnedUTXO (UTXO txid vout _) _) -> 
                (txid, vout) `Map.member` utxoPool) utxos
            && Map.size utxoPool == length utxos
        Nothing -> False

