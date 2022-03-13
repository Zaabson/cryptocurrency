{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module BlockValidation where

import Data.List (mapAccumL)
import Control.Monad.State.Lazy
import Control.Monad (forM)
import Control.Arrow ((>>>), (&&&))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.Map as Map
import qualified Codec.Crypto.RSA as RSA
import BlockType
import Hashing (HashOf(..), shash256, toRawHash, TargetHash(..), RawHash (RawHash), shashBytes)
import Data.Bifunctor (first)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
-- import BlockChain (LivelyBlocks(..), linkToChain)

x |> f = f x
infixl 1 |>

data UTXO = UTXO TXID Integer Output -- TODO use this type below (or don't)
    deriving (Show, Generic)

instance ToJSON UTXO
instance FromJSON UTXO 

instance Eq UTXO where
    (UTXO txid1 vout1 _) == (UTXO txid2 vout2 _) = (txid1 == txid2) && (vout1 == vout2)

newtype UTXOPool = UTXOPool (Map.Map (TXID, Integer) Output)
    deriving (Show)

searchPool :: UTXOPool -> [Input] -> [Maybe Output]
searchPool (UTXOPool pool) = map (\i -> Map.lookup (utxoReference i, vout i) pool)

searchPool' :: UTXOPool -> [Input] -> [Maybe UTXO]
searchPool' (UTXOPool pool) = map $ \i -> do
    let (txid, index) = (utxoReference i, vout i)
    output <- Map.lookup (txid, index) pool
    Just $ UTXO txid index output


sumMoney :: [Output] -> Cent
sumMoney = sum . map outputDenomination

-- each input is signed seperately.
-- Modified transaction is created with only one input - that is input for signing.
-- signature field in input is set to empty bytestring, 
-- signerPublicKey field needs to hash to a matching ownerPublicAddress field in referenced output.
-- Such transaction is hashed and signed.
createSignedInput :: Transaction     -- transaction to be signed, inputs don't matter, outputs are signed
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
        rightAmount unspend = sumMoney unspend >= sumMoney (outputs tx)

-- this function is simmilar to the one above^
-- it doesn't make sense to do this calculation (searching the UTXOPool) twice
-- but I'll wait with changing it till I see how it's used in the running app
-- validTransactionFee :: UTXOPool -> Transaction -> Maybe Cent  
-- validTransactionFee pool tx = case sequenceA $ searchPool pool (inputs tx) of
--     Nothing -> Nothing
--     Just unspend -> Just $ fee unspend
--     where 
--         fee unspend = sumMoney unspend - sumMoney (outputs tx)

-- !! Bug: doesn't remove referenced UTXOs from pool 
-- Leave this implementation for testing
-- first transaction is coinbase transaction.
-- valid block needs to have at least coinbase transaction.
validateBlockTransactions' :: UTXOPool -> Block -> (UTXOPool, Bool)
validateBlockTransactions' pool Block{transactions=[], ..} = (pool, False)
validateBlockTransactions' (UTXOPool pool) Block{transactions=txs, ..} = first UTXOPool $ foldl f (pool, True) txs
    where f (pool, bool) tx = (foldl (flip $ uncurry Map.insert) pool (kvpairs tx), bool && validTransaction (UTXOPool pool) tx)
          kvpairs tx = snd $ mapAccumL (\n o -> (n+1, ((shash256 $ Right tx, n), o))) 0 (outputs tx)

-- !!! DONT FORGET COINBASE          

-- Validates all transactions in a block (in order) updating UTXOPool on the way.
-- returns (True, UTXOPool updated by transactions in this block without coinbase) or (False, Junk)
-- N̶o̶t̶e̶:̶ ̶C̶o̶i̶n̶b̶a̶s̶e̶ ̶m̶o̶n̶e̶y̶ ̶c̶a̶n̶n̶o̶t̶ ̶b̶e̶ ̶c̶r̶e̶a̶t̶e̶d̶ ̶a̶n̶d̶ ̶s̶p̶e̶n̶t̶ ̶i̶n̶ ̶t̶h̶e̶ ̶s̶a̶m̶e̶ ̶b̶l̶o̶c̶k̶,̶ ̶s̶o̶m̶e̶ ̶n̶u̶m̶b̶e̶r̶ ̶(̶t̶o̶ ̶b̶e̶ ̶s̶p̶e̶c̶i̶f̶i̶e̶d̶ ̶l̶a̶t̶e̶r̶)̶ ̶o̶f̶ ̶b̶l̶o̶c̶k̶s̶ ̶n̶e̶e̶d̶s̶ ̶t̶o̶ ̶b̶e̶ ̶w̶a̶i̶t̶e̶d̶
-- ^ let's say in general outputs can't be spend in the same block they're created, but the next block there's no restrictions and coinbase can be spend.
-- ^ ey, but it's not how this function works: an output from earlier transaction can be used as an input in a later one.
validateBlockTransactions :: UTXOPool -> Block -> (Bool, UTXOPool)
validateBlockTransactions (UTXOPool pool) Block{transactions=txs, coinbaseTransaction=coinbase} = 
        let (bl, utxoPool) = runState (validate txs) pool in
        -- also add coinbase UTXOs; TODO refactor
        if bl then
            (True, UTXOPool $ foldl (\m (UTXO txid vout out) -> Map.insert (txid, vout) out m) utxoPool $ coinbaseGetNewUTXOs coinbase)
        else
            (False, UTXOPool utxoPool)
    where validate :: [Transaction] -> State (Map.Map (TXID, Integer) Output) Bool
          validate [] = return True
          validate (tx : rest) = do
                pool <- get
                let bool = validTransaction (UTXOPool pool) tx
                let hash = shash256 $ Right tx
                -- remove UTXOs that were referenced in inputs
                let pool1 = foldl (flip Map.delete) pool $ map (utxoReference &&& vout) $ inputs tx
                -- add UTXO created in outputs
                let kv   = tx |> outputs |> zip [0..] |> map (\case (n, o) -> ((hash, n), o))
                let pool2 = foldl (flip $ uncurry Map.insert) pool1 kv
                if not bool then
                    return False
                else do 
                    put pool2
                    (bool &&) <$> validate rest

txGetNewUTXOs :: Transaction -> [UTXO]
txGetNewUTXOs tx =
    let hash = shash256 $ Right tx in
    outputsToUTXOs hash (outputs tx)

coinbaseGetNewUTXOs :: Coinbase -> [UTXO]
coinbaseGetNewUTXOs coinbase = 
    let hash = shash256 $ Left coinbase in
    outputsToUTXOs hash (coinbaseOutputs coinbase)

outputsToUTXOs :: TXID -> [Output] -> [UTXO]
outputsToUTXOs txid outs = 
    outs |> zip [0..] |> map (\case (n, o) -> UTXO txid n o)

blocksPerHalving :: Integer
blocksPerHalving = 100000
-- Block number 2600000 is the first that doesn't mine new coins

calculateBlockReward :: Integer -> Cent
-- calculateBlockReward blockHeight = Cent $ floor(100000000 * (0.5 :: Double) ^ ceiling ((fromIntegral blockHeight + 1)/ fromIntegral blocksPerHalving))
calculateBlockReward = const (Cent 15)

-- used in block validation by block receiving nodes
-- checks indirectly if transaction fees and block reward were calculated correctly
validateCoinbaseMoney :: UTXOPool -> Block -> Bool
validateCoinbaseMoney pool Block{transactions=txs, coinbaseTransaction=coinbase, blockHeader=BlockHeader{..}} = validCoinbase
    where outputsMoney  = sumMoney (concatMap outputs txs) + sumMoney (coinbaseOutputs coinbase)
          mutxos   = txs |>  concatMap inputs |> searchPool pool |> sequenceA
          validCoinbase = case mutxos of
                Nothing    -> False   -- utxo not found in UTXOPool
                Just utxos -> outputsMoney <= sumMoney utxos + calculateBlockReward (blockHeight coinbase)

validateNonce :: TargetHash -> BlockHeader -> Bool
validateNonce (TargetHash targetHash) blck = toRawHash (shash256 blck) <= targetHash

-- This possibly doesn't need to check linkToBlockchain <- done.
-- validateBlock :: TargetHash -> UTXOPool -> Block -> Bool
-- validateBlock targetHash pool block@Block{..} = txsOk && coinbaseOk && nonceOk          -- && blockchainOk 
--     where (txsOk, newPool) = validateBlockTransactions pool block
--           coinbaseOk       = validateCoinbaseMoney pool block
--           nonceOk          = validateNonce targetHash blockHeader
--         --   blockchainOk     = case linkToChain block (getLivelyBlocks blocks) of
--         --                             Nothing -> False
--         --                             Just _  -> True

-- pairs elements in a list, pair by pair, duplicates the last element if the list is odd length 
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = [(x, x)]
pairs (x:y:xs) = (x, y) : pairs xs 

-- calculates a merkle root hash of a list by first hashing a transactions in a list
-- and then pairing neighbouring hashes and combining them into single hash iteratively - until there's a single hash, root hash
-- coinbase transaction is appended at the beginning
merkleHash :: Coinbase -> [Transaction] -> RawHash
merkleHash coinbase = map (shash256 >>> getHash) >>> 
    ((coinbase |> shash256 |> getHash) :) >>> untilOneLeft pairAndHash >>> head >>> RawHash
    where pairAndHash :: [B.ByteString] -> [B.ByteString]
          pairAndHash = pairs >>> map (uncurry B.append >>> shashBytes)
          untilOneLeft _ [x] = [x]
          untilOneLeft f xs  = untilOneLeft f $ f xs

-- TODO: maybe some searching through pool can be optimized combining validateCoinbasae and validateTransactions
-- This possibly doesn't need to check linkToBlockchain <- done.
validateBlock :: TargetHash -> UTXOPool -> Block -> (Bool, UTXOPool)
validateBlock targetHash pool block@Block{..} = (txsOk && coinbaseOk && nonceOk && merkleOk, newPool)          -- && blockchainOk 
    where (txsOk, newPool) = validateBlockTransactions pool block
          coinbaseOk       = validateCoinbaseMoney pool block
          nonceOk          = validateNonce targetHash blockHeader
          merkleOk         = validateMerkleHash block

-- validateFirstBlock :: TargetHash 

-- Validate what can be without utxoPool. Used by lightnode.
-- Validates 
-- partialValidateBlock :: TargetHash -> Block -> Bool
-- partialValidateBlock targetHash block@Block{coinbaseTransaction=coinbase, transactions=txs,  ..} = nonceOk && merkleOk          -- && blockchainOk 
--     where nonceOk          = validateNonce targetHash blockHeader
--           merkleOk         = merkleHash coinbase txs == blockRootHash block 

validateMerkleHash :: Block -> Bool
validateMerkleHash block@Block{coinbaseTransaction=coinbase, transactions=txs,  ..} = merkleHash coinbase txs == blockRootHash block 

-- For use in light node. 
validateNonceAndMerkle :: TargetHash -> Block -> Bool
validateNonceAndMerkle targetHash block = 
    validateNonce targetHash (blockHeader block) && validateMerkleHash block