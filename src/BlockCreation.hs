module BlockCreation where

import Merkle
import Hashing (RawHash (RawHash), HashOf(..), shash256, toRawHash, TargetHash(TargetHash))
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.ByteString as B
import Data.Time (UTCTime, getCurrentTime)
import Data.List (find)
import Control.Arrow ((&&&))
import Control.DeepSeq
import Data.Maybe (fromJust)
import Data.Bifunctor (second)
import qualified Codec.Crypto.RSA as RSA
import Crypto.Random (CryptoRandomGen)
import BlockType (blockBlockHeight, TXID, Output(..), Transaction(..), Input,
                  BlockReference, BlockHeader(..), Coinbase(..), Block(..), Cent(..), Genesis, PublicAddress)
import BlockValidation (calculateBlockReward, createSignedInput, UTXO(..), txGetNewUTXOs)

-- Idea:
--   - transaction creation environment/monad
--   - collects created along the way OwnedUTXOs (i.e. change)
--   - stores RandomGen in the background to be able to generate key pairs
-- ^ naaah, transaction is no matter what created at one go knowing [OwnedUTXO] and [Output]
--   and maybe returning change  

data Keys = Keys RSA.PublicKey RSA.PrivateKey

data OwnedUTXO = OwnedUTXO UTXO Keys

instance Show OwnedUTXO where
    show (OwnedUTXO utxo _) = "Owned: " ++ show utxo

howMuchCash :: OwnedUTXO -> Cent
howMuchCash (OwnedUTXO (UTXO _ _ (Output cents _)) _) = cents

createSignedInputFromUTXO :: Transaction -> OwnedUTXO -> Input
createSignedInputFromUTXO tx (OwnedUTXO (UTXO txid vout _) (Keys pub priv)) = createSignedInput tx txid vout pub priv

-- creates a transaction sending coins to an address
-- and the surplus/change to new Address 
-- arguments are:    recipient's address, amount, money to spend -> (change, tx)    
-- createSimpleTransaction :: PublicAddress -> Cent -> OwnedUTXO -> (OwnedUTXO, Transaction) 
-- createOneOutputTransaction recipient amount utxo =
--     let tx = Transaction [] [Output amount recipient] in
--     let  input = createSignedInputFromUTXO tx utxo in 
--     tx {inputs = [input]}

-- creates inputs from OwnedUTXOs signed for the given outputs
-- puts inputs and outputs into transaction
-- outputs go into transaction in the unchanged order, can expect vout in tx to match index in the argument list 
createTransaction :: [OwnedUTXO] -> [Output] -> Transaction
createTransaction utxos outs =
    let tx = Transaction [] outs in
    let inputs = map (createSignedInputFromUTXO tx) utxos in 
    tx {inputs = inputs}

-- returns a prefix of a list consisting of every element where the "predicate" returned True and the rest of the list,
-- predicate accumulates helper state, last state for which predicate returned True is returned
takeTillWithAccum :: (a -> b -> (a, Bool)) -> a -> [b] -> (a, [b], [b])
takeTillWithAccum f s [] = (s, [], [])
takeTillWithAccum f s (x:xs) = 
    let (s', bl) = f s x
    in if bl then
        second (x :) $ takeTillWithAccum f s' xs
    else
        (s, [], x : xs) -- !! s not s'

type SimpleWallet = [OwnedUTXO]


createSendingTransaction :: SimpleWallet                 -- collection of OwnedUTXOs to be used as inputs
                         -> Keys                         -- sender's keys to be used for change
                         -> PublicAddress                -- recipient's public address
                         -> Cent                         -- amount to be sent  
                         -> Maybe (SimpleWallet, Transaction)   -- (unused UTXOs + change 2UTXO, created transaction)
createSendingTransaction ownedUTXOs keys@(Keys pub priv) recipient amount =
    case takeTillWithAccum (\s utxo -> 
            let s' = howMuchCash utxo + s in 
                (s', s' < amount)) 0 ownedUTXOs of 
        (s, notEnough, []) -> Nothing
        (s, notEnough, u:us) -> 
            let change = Output {outputDenomination=s+howMuchCash u - amount, ownerPublicAddress=shash256 pub} in 
            let tx = Transaction [] [Output {outputDenomination=amount, ownerPublicAddress=recipient}, 
                                     change] in 
            let tx = tx {inputs=map (createSignedInputFromUTXO tx) (u:notEnough)}
            in Just (OwnedUTXO (UTXO (shash256 (Right tx)) 1 change) keys : us, tx)


keysLength :: Int
keysLength = 1024

generateKeys :: CryptoRandomGen g =>  g -> Keys
generateKeys g = Keys pub priv
    where (pub, priv, _) = RSA.generateKeyPair g keysLength 

-- Codec.Crypto.RSA
-- generateKeyPair :: RandomGen g => g -> Int -> (PublicKey, PrivateKey, g)

-- heats up till it finds nonce producing hash not greater than target
crunchNonce :: TargetHash -> RawHash -> UTCTime -> BlockReference -> BlockHeader
crunchNonce (TargetHash target) merklehash timestamp prevhash = 
    fromJust $ find (\b -> toRawHash (shash256 b) <= target) $ map blockWithNonce [0..]
    where
        baseblock = BlockHeader {
            nonce=0, 
            previousHash=prevhash,
            timestamp=timestamp,
            rootHash=merklehash
            }
        blockWithNonce nonce = baseblock {nonce=nonce}

-- Crunches hashes to find nonce, creates a block with given data.
-- Doesn't include fees!!! TODO
mineBlock :: TargetHash               -- target hash
          -> Keys                  -- keys for coinbase output
          -> UTCTime               -- creation time
          -> [Transaction]         -- transactions to include
          -> Integer               -- new block height
          -> BlockReference        -- reference to previous block
          -> (OwnedUTXO, Block)    -- coinbase UTXO, new block
mineBlock target keys@(Keys pub _) timestamp txs newBlockHeight prevblockref = 
        blockhdr `deepseq` 
            (OwnedUTXO (UTXO coinbaseTxid 0 coinbaseOutput) keys,
            Block blockhdr coinbase txs)
    where
        -- create a coinbase transaction spending all mining reward in a single output for given keys
        reward = calculateBlockReward newBlockHeight
        coinbaseOutput = Output reward (shash256 pub)
        coinbase = Coinbase newBlockHeight [coinbaseOutput]
        coinbaseTxid = shash256 $ Left coinbase
        
        -- hash transactions and coinbase transaction producing roothash/merklehash
        merklehash = merkleHash coinbase txs
        
        -- mine, crunch hashes to find a matching nonce to put into blockHeader
        blockhdr = crunchNonce target merklehash timestamp prevblockref

blockRef :: Block -> BlockReference
blockRef = shash256 . Right . blockHeader

-- convenience to mine a block appending to a given previous block
-- mineAfterBlock :: Block -> TargetHash -> Keys -> UTCTime -> [Transaction] -> (OwnedUTXO, Block)
-- mineAfterBlock prevblock target keys timestamp txs = 
--     mineBlock target keys timestamp txs (blockBlockHeight prevblock + 1) (blockRef prevblock)

-- mineAfterGenesis :: Genesis -> TargetHash -> Keys -> UTCTime -> [Transaction] -> (OwnedUTXO, Block)
-- mineAfterGenesis genesis target keys timestamp txs = 
--     mineBlock target keys timestamp txs 1 (shash256 $ Left genesis)

-- not needed right? <- nope
-- First mined block doesn't have any transactions as there's no money to spend yet,
-- it creates the first existing money in coinbase 
-- mineFirst :: RawHash -> Keys -> UTCTime -> Genesis -> (OwnedUTXO, Block)
-- mineFirst target keys@(Keys pub _) timestamp genesis = 
--         (OwnedUTXO (UTXO coinbaseTxid 0 coinbaseOutput) keys,
--          Block blockhdr coinbase [])
--     where
--         newBlockHeight = 1
--         reward = calculateBlockReward newBlockHeight
--         coinbaseOutput = Output reward (shash256 pub)
--         coinbase = Coinbase newBlockHeight [coinbaseOutput]
--         coinbaseTxid = shash256 $ Left coinbase
--         blockhdr = mine target timestamp coinbase [] (shash256 $ Left genesis)
