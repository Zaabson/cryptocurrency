{-# LANGUAGE RecordWildCards, LambdaCase #-}

module BlockValidation where

import Data.List (mapAccumL)
import Control.Monad.State.Lazy
import Control.Monad (forM)
import Control.Arrow ((>>>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.Map as Map
import qualified Codec.Crypto.RSA as RSA
import BlockType
import Hashing (HashOf(..), shash256)

x |> f = f x
infixl 1 |>

instance Eq (HashOf a) where 
    (Hash b) == (Hash c) = b == c
instance Ord (HashOf a) where
    compare (Hash b) (Hash s) = compare b s

type UTXOPool = Map.Map (TXID, Integer) Output

searchPool :: UTXOPool -> [Input] -> [Maybe Output]
searchPool pool = map (\i -> Map.lookup (utxoReference i, vout i) pool)

sumMoney :: [Output] -> Cent
sumMoney = sum . map outputDenomination

-- each input is signed seperately.
-- Modified transaction is created with only one input - that is input for signing.
-- signature field in input is set to empty bytestring, 
-- signerPublicKey field needs to hash to a matching ownerPublicAddress field in referenced output.
-- Such transaction is hashed and signed.
createSignedInput :: Transaction     -- transaction to be signed, inputs don't matter, outputs and blockHeight are signed
                  -> TXID            -- reference to UTXO
                  -> Integer         -- vout in referenced UTXO
                  -> RSA.PublicKey
                  -> RSA.PrivateKey  
                  -> Input
createSignedInput tx utxid vout pubKey privKey = unsignedInput {signature=signature}  where 
    unsignedInput = Input {signature=Signature B.empty, signerPublicKey=pubKey, utxoReference=utxid, vout=vout}
    signature = Signature . LazyB.toStrict . RSA.sign privKey $ hashWithInput tx unsignedInput

hashWithInput :: Transaction -> Input -> LazyB.ByteString
hashWithInput tx inp = LazyB.fromStrict . getHash $ shash256 (tx {inputs = [inp]})

verifyInputSignatures :: Transaction -> Bool
verifyInputSignatures tx = all rightSignature (inputs tx)
    where rightSignature inp = RSA.verify (signerPublicKey inp) 
                                          (hashWithInput tx (inp {signature = Signature B.empty}))
                                          (case signature inp of Signature bytes -> LazyB.fromStrict bytes)

-- valid non-coinbase transaction
-- for use of a miner
validTransaction :: UTXOPool -> Transaction -> Bool
validTransaction pool tx = case sequenceA $ searchPool pool (inputs tx) of 
    Nothing -> False
    Just unspend -> rightAmount unspend && verifyInputSignatures tx
    where 
        rightAmount unspend =  sumMoney unspend >= sumMoney (outputs tx)

-- Leave this implementation for testing
-- first transaction is coinbase transaction.
-- valid block needs to have at least coinbase transaction.
validateBlockTransactions' :: UTXOPool -> Block -> (UTXOPool, Bool)
validateBlockTransactions' pool Block{transactions=[], ..} = (pool, False)
validateBlockTransactions' pool Block{transactions=txs, ..} = foldl f (pool, True) txs
    where f (pool, bool) tx = (foldl (flip $ uncurry Map.insert) pool (kvpairs tx), bool && validTransaction pool tx)
          kvpairs tx = snd $ mapAccumL (\n o -> (n+1, ((shash256 tx, n), o))) 0 (outputs tx)

-- !!! DONT FORGET COINBASE          

-- Validates all transactions in a block in order updating UTXOPool on the way.
-- returns (True, UTXOPool updated by transactions in this block without coinbase) or (False, Junk)
-- Note: Coinbase money cannot be created and spent in the same block, some number (to be specified later) of blocks needs to be waited
validateBlockTransactions :: UTXOPool -> Block -> (Bool, UTXOPool)
validateBlockTransactions pool Block{transactions=txs, ..} = runState (validate txs) pool
    where validate :: [Transaction] -> State UTXOPool Bool
          validate [] = return True
          validate (tx : rest) = do pool <- get
                                    let bool = validTransaction pool tx
                                    let hash = shash256 tx
                                    let kv   = tx |> outputs |> zip [0..] |> map (\case (n, o) -> ((hash, n), o))
                                    if not bool 
                                    then return False
                                    else do put $ foldl (flip $ uncurry Map.insert) pool kv
                                            (bool &&) <$> validate rest

blocksPerHalving :: Integer
blocksPerHalving = 100000
-- Block number 2600000 is the first that doesn't mine new coins

calculateBlockReward :: Integer -> Cent
calculateBlockReward blockHeight = Cent $ floor(100000000 * (0.5 :: Double) ^ ceiling ((fromIntegral blockHeight + 1)/ fromIntegral blocksPerHalving))

-- used in block validation by block receiving nodes
-- checks indirectly if transaction fees and block reward were calculated correctly
validateCoinbaseMoney :: UTXOPool -> Block -> Bool
validateCoinbaseMoney pool Block{transactions=txs, coinbaseTransaction=coinbase, blockHeader=BlockHeader{..}} = validCoinbase
    where outputsMoney  = sumMoney (concatMap outputs txs) + sumMoney (coinbaseOutputs coinbase)
          mutxos   = txs |>  concatMap inputs |> searchPool pool |> sequenceA
          validCoinbase = case mutxos of
                Nothing    -> False   -- utxo not found in UTXOPool
                Just utxos -> outputsMoney <= sumMoney utxos + calculateBlockReward (blockHeight coinbase)


validateBlock :: UTXOPool -> Block -> Bool
validateBlock pool block@Block{..} = txsOk && coinbaseOk
    where (txsOk, newPool) = validateBlockTransactions pool block
          coinbaseOk       = validateCoinbaseMoney pool block
          -- blockchainOk  = linkToBlockchain $ blockRootHash block

-- Ideas:
--     - Blockchain typeclass that combines for example: blockchain with only headers, blockchain tree, 
--                                                       (maybe UTXOPool as it also stores blockchain state, maybe record with UTXOPool)
--     - should validateBlock function be a state/reader monad, reads and changes blockchain state?
