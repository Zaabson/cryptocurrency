{-# LANGUAGE RecordWildCards, LambdaCase #-}

-- idea: blockHeight field is really only needed in coinbase transaction,
-- what is an argument to seperate this tx as second type/constructor

module BlockValidation where

import Data.List (mapAccumL)
import Control.Monad.State.Lazy
import Control.Arrow ((>>>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.Map as Map
import Control.Monad (forM)
import qualified Codec.Crypto.RSA as RSA
import BlockType

x |> f = f x
infixl 1 |>

instance Eq (HashOf a) where 
    (Hash b) == (Hash c) = b == c
instance Ord (HashOf a) where
    compare (Hash b) (Hash s) = compare b s

type UTXOPool = Map.Map (TXID, Integer) Output

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
hashWithInput tx inp = LazyB.fromStrict . rawHash $ shash256 (tx {inputs = [inp]})

verifyInputSignatures :: Transaction -> Bool
verifyInputSignatures tx = all rightSignature (inputs tx)
    where rightSignature inp = RSA.verify (signerPublicKey inp) 
                                          (hashWithInput tx (inp {signature = Signature B.empty}))
                                          (case signature inp of Signature bytes -> LazyB.fromStrict bytes)

-- valid non-coinbase transaction
validTransaction :: UTXOPool -> Transaction -> Bool
validTransaction pool tx = case mapM (`Map.lookup` pool) unspendRefs of 
    Nothing -> False
    Just unspend -> rightAmount unspend && verifyInputSignatures tx
    where 
        unspendRefs = map (\i -> (utxoReference i, vout i)) (inputs tx)
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
                  
--- okkey , dumbas cents are indivisible
calculateBlockReward :: Integer -> Cent
calculateBlockReward blockHeight = Cent $ floor(100000000 * (0.5 :: Double) ^ ceiling (fromIntegral blockHeight / fromIntegral blocksPerHalving))

validateBlock :: UTXOPool -> Block -> Bool
validateBlock pool Block{transactions=txs, coinbaseTransaction=coinbase, blockHeader=BlockHeader{..}} = validCoinbase
    where validCoinbase = sumMoney (concatMap outputs txs) + 69 <= 420