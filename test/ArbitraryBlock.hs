{-# LANGUAGE ScopedTypeVariables #-}
module ArbitraryBlock where

import Test.QuickCheck
import BlockType (Transaction(..), Input (utxoReference), Output (Output), BlockHeader(..), Genesis(..), BlockReference, Block(..), Cent(..), Coinbase (blockHeight))
import qualified Codec.Crypto.RSA as RSA
import qualified Crypto.Random.DRBG as DRBG
import Data.ByteString (ByteString, pack)
import Data.Word (Word8)
import BlockCreation (Keys(..), OwnedUTXO(..), howMuchCash, createTransaction, mineBlock, blockRef)
import BlockValidation (UTXO(..))
import Hashing (shash256, RawHash(..), TargetHash(..))
import Math.Combinat.Partitions (randomPartition, fromPartition)
import Data.List (mapAccumL)
import Test.QuickCheck.Random (QCGen, Splittable (right))
import Test.QuickCheck.Instances.Time


-- argument is number of bytes
arbitraryByteString :: Int -> Gen ByteString
arbitraryByteString len = do
    nums <- vector len :: Gen [Word8]
    return $ pack nums

makeRandomGen :: Gen DRBG.HmacDRBG
makeRandomGen =  do
        entropy <- arbitraryByteString 1024
        let Right (g :: DRBG.HmacDRBG) = DRBG.newGen entropy
        return g

-- testKeyLength = 256
testKeyLength = 512


instance Arbitrary Keys where
    arbitrary = do
        g <- makeRandomGen
        let (pub, priv, _) = RSA.generateKeyPair g testKeyLength
        return $ Keys pub priv

data InpOut = InpOut Input Output

-- instance Arbitrary InpOut where
--     arbitrary = do
--         Keys pub priv <- arbitrary

-- create transactions using all provided UTXOs and return UTXOs introduced in the transactions
-- arbitraryValidTransactions :: [OwnedUTXO] -> ([OwnedUTXO], [Transaction])
-- arbitraryValidTransactions utxos = do

instance Arbitrary Genesis where
    arbitrary = Genesis <$> arbitrary

-- create an arbitrary output that spends in the range [maxSpend/2, maxSpend]
-- arbitraryOutputWithCap :: Cent -> Gen (Output, OwnedUTXO)
-- arbitraryOutputWithCap maxSpend = do
--     Keys pub priv <- arbitrary

-- creates a transaction spending given UTXOs, returns newly created UTXOs
-- first amount of money thats going to be spend in outputs is set,
-- then the money is randomly partitioned into outputs
arbitraryTransaction :: [OwnedUTXO] -> Gen ([OwnedUTXO], Transaction)
arbitraryTransaction utxos = do
    spendMoney <- choose (0, fromEnum inputsMoney) :: Gen Int
    crgen <- makeRandomGen
    rgen  <- arbitrary :: Gen QCGen
    let keyOutputPairs = moneyPartition spendMoney rgen crgen
    let tx = createTransaction utxos $ map snd keyOutputPairs
    let txid = shash256 $ Right tx
    let owned = zipWith (\vout (keys, out) -> OwnedUTXO (UTXO txid vout out) keys) [0..] keyOutputPairs
    return (owned, tx)

    where
        Cent inputsMoney = sum $ map howMuchCash utxos

        f g n = let (pub, priv, g') = RSA.generateKeyPair g testKeyLength
                in (g', (Keys pub priv, Output (Cent n) (shash256 pub)))

        moneyPartition spendMoney rgen crgen =
            case randomPartition spendMoney rgen of
                (partition, _) -> snd $ mapAccumL f crgen $ map toEnum (fromPartition partition)


arbitraryPartitionShuffle :: forall a. [a] -> Gen [[a]]
arbitraryPartitionShuffle xs =
    arbitrary >>= partShuffle
    where
        partitionList [] [] = []
        partitionList (n:ns) ys = take n ys : partitionList ns (drop n ys) 

        partShuffle :: QCGen -> Gen [[a]]
        partShuffle g =
            let (partition, _) = randomPartition (length xs) g in 
            shuffle $ partitionList (fromPartition partition) xs

testTargetHash :: TargetHash
-- testTargetHash = RawHash $ pack $ 0 : replicate 31 255 
testTargetHash = TargetHash . RawHash . pack $ 0 : replicate 31 255

-- Creates a new block with a given height and previous blockReference;
-- block is created using some random share of provided OwnedUTXOs to create transactions;
-- UTXOs are partitioned arbitrarily into transactions. 
-- Newly created OwnedUTXOs and the ones that were not used are returned as well
arbitraryBlock :: BlockReference -> Integer -> [OwnedUTXO] -> Gen ([OwnedUTXO], Block)
arbitraryBlock prevblockref blockheight utxos = do

    usedUtxos <- sublistOf utxos
    
    let usedUtxos'  = map (\(OwnedUTXO utxo _) -> utxo) usedUtxos
    let unusedUtxos = filter (\(OwnedUTXO utxo _) -> utxo `notElem` usedUtxos') utxos         -- slow? sublistOf is inorder so can improve it to O(n)
    
    txUtxos <- arbitraryPartitionShuffle usedUtxos
    
    pairs <- mapM arbitraryTransaction txUtxos
    let (newUtxos, txs) = unzip pairs

    coinbaseKeys <- arbitrary
    time         <- arbitrary   -- for now time is not a part of the protocol, can be arbitrary
    -- the block is actualy mined, take trivial target hash for this to run quick
    let (coinbaseUtxo, newBlock) = mineBlock testTargetHash coinbaseKeys time txs blockheight prevblockref

    return (coinbaseUtxo : unusedUtxos ++ (concat newUtxos) , newBlock)

data BlockchainWithState = BlockchainWithState [OwnedUTXO] [Block] Genesis
    deriving (Show)

instance Arbitrary BlockchainWithState where
    arbitrary = do
        (utxos, blocks, genesis) <- arbitraryBlockchain
        return $ BlockchainWithState utxos blocks genesis

-- Creates arbitrary blockchain.
-- Each turn a random selection of UTXOs is used to create a new block,
-- first block is the newest and last block is first,
-- created UTXOs are accumulated along the way.
arbitraryBlockchain :: Gen ([OwnedUTXO], [Block], Genesis)
arbitraryBlockchain = sized $ \maxheight -> do
        genesis <- arbitrary
        (utxos, blockchain) <- makeBlockchain (toEnum maxheight) [] (shash256 $ Left genesis) 1
        return (utxos, blockchain, genesis)

    where
        makeBlockchain :: Integer                     -- height of a last block to be created
                       -> [OwnedUTXO]                 -- UTXOs after block created previously
                       -> BlockReference              -- reference to a block created previously
                       -> Integer                     -- height of a block to create next
                       -> Gen ([OwnedUTXO], [Block]) 
        makeBlockchain maxheight utxos prevblockref blockheight =
            if blockheight <= maxheight then do
                (utxos', block) <- arbitraryBlock prevblockref blockheight utxos
                
                let blockref = blockRef block

                (futureUtxos, blockchain) <- makeBlockchain maxheight utxos' blockref (blockheight + 1)
                return (futureUtxos, blockchain ++ [block])   -- append a block at the end
            else
                return (utxos, [])
