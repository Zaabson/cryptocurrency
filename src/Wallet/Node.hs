{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wallet.Node where

import MessageHandlers (MessageHandler, ignoreBlockchainQuery, ignoreTransaction, combineHandlers, answerPing, answerContactQuery, MsgHandler (MsgHandler))
import MessageType (Answer (BlockAnswer), ReceivedBlock (ReceivedBlock), Message (BlockMessage))
import BlockType (Block (blockHeader, transactions), BlockHeader (BlockHeader), Coinbase (blockHeight))
import qualified InMemory
import Node (PeersSet, broadcastAndUpdatePeers, catchUpToBlockchain, AppendFixed (appendFixed))
import BlockChain (BlockchainUpdated(BlockInserted, FutureBlock, BlockAlreadyInserted, BlockInvalid, BLockInsertedLinksToRoot), ForkMaxDiff, updateWithBlockHeader, Fixed (Fixed), Lively (Lively), Future (Future))
import Control.Concurrent (forkIO)
import Control.Monad (join)
import InMemory (runAtomically, HasLogging (logger), InMemory (writeMemory, readMemory, modifyMemory), InMemoryRead (readMemoryIO))
import Hashing (TargetHash, shash256)
import qualified Data.Set as Set
import BlockValidation (validateNonceAndMerkle)
import Wallet.Type (Status(Validated))
import Wallet.Session (updateTxStatusMany, updateStatusByBlock, addFixedBlockHeader)
import Hasql.Session (Session)
import Hasql.Transaction.Sessions (transaction, IsolationLevel (Serializable), Mode (Write))
import BlockCreation (blockRef)
import Control.Concurrent.Async (forConcurrently_)
import Data.Int (Int64)

class HasDB appState where
    executeDB :: appState -> Session a -> IO a

-- Blockheight is kept in coinbase, absent in blockheader.
-- TODO: Fix resulting ineffectivenes.
whatsNextBlock :: InMemory appState m (Fixed BlockHeader) => appState -> IO Integer
whatsNextBlock appState = do
    Fixed (fixed :: [BlockHeader]) <- readMemoryIO appState
    return . fromIntegral $ 1 + length fixed


-- Validate the block (nonce and merkle hash). 
-- If valid return a db session updating status to validated for all tracked transactions from the block 
-- and adding the block to database. 
aknowledgeFixedBlock :: TargetHash -> Block -> Maybe (Session ())
aknowledgeFixedBlock target block =
    if validateNonceAndMerkle target block then
        let txset = Set.fromList (map (shash256 . Right) $ transactions block) in
            -- Only DB
            Just $ transaction Serializable Write $ do
                updateStatusByBlock (blockRef block) (`Set.member` txset) Validated
                addFixedBlockHeader (blockHeader block)
    else
        Nothing

-- Length of fixedBlocks, will probably be kept in TVar.
-- In fullNode it was just read from the last fixed block.
-- But height is stored in coinbase (so that coinbase txs hash differently in each block)
-- so a light node doesn't know fixedBlocks height if it doesn't store this info.
newtype FixedLength = FixedLength {getFixedLength :: Int64}
    deriving (Num, Eq, Ord)

lightNodeCatchUpToBlockchain ::  (HasLogging appState,
    InMemoryRead appState FixedLength,
    HasDB appState,
        InMemory appState m PeersSet)
    =>  ForkMaxDiff -> TargetHash -> appState -> IO ()
lightNodeCatchUpToBlockchain forkMaxDiff targetHash appState = do
    blocks <- catchUpToBlockchain forkMaxDiff targetHash ( fmap (toInteger . getFixedLength . (+ FixedLength 1)) . readMemoryIO) appState
    forConcurrently_ blocks $ maybe (return ()) (executeDB appState) . aknowledgeFixedBlock targetHash

receiveBlockLight :: (HasLogging appState,
    InMemory appState m (Lively BlockHeader),  -- STM
    AppendFixed appState m BlockHeader,        -- DB
    InMemory appState m (Future BlockHeader),  -- STM
    -- HasDB appState,                         
    InMemory appState m PeersSet, InMemory appState m FixedLength, HasDB appState)
    => ForkMaxDiff -> TargetHash -> appState -> MsgHandler Block ReceivedBlock
receiveBlockLight forkMaxDiff targetHash appState = MsgHandler $ \block -> do

    forkIO . join . runAtomically $ do
    -- Note that this atomical operation is time-consuming. TODO: Benchmark how big of a problem that is.
        lively   <- readMemory appState -- stm
        -- fixed    <- readMemory appState -- db
        future   <- readMemory appState -- stm

        -- try to link a new block to one of the recent blocks
        case updateWithBlockHeader forkMaxDiff targetHash (blockHeader block) lively future of
            BlockInserted lively' newfixed -> do
                writeMemory appState lively' -- stm
                appendFixed appState newfixed  -- db, also different interface to only add new blocks. 
                modifyMemory appState (+ FixedLength (fromIntegral $ length newfixed))
                return $ do
                    logger appState "handler: Received block was inserted into chain."
                    -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                    broadcastAndUpdatePeers appState (BlockMessage block) (BlockAnswer ReceivedBlock)
            FutureBlock future'   -> do
                writeMemory appState future'  -- stm
                return $ do
                    logger appState "handler: Received block inserted into futures waiting list."
                    -- We received a block that doesn't link to a known recent chain. Let's query for new blocks.
                    lightNodeCatchUpToBlockchain forkMaxDiff targetHash appState
            BlockAlreadyInserted -> return $ logger appState "handler: Received block was already present in the chain."
            BlockInvalid         -> return $ logger appState "handler: Received block is invalid."
            BLockInsertedLinksToRoot lively' -> do
                writeMemory appState lively' -- stm
                return $ do
                    logger appState "handler: Inserted block linking to genesis."
                    -- Broadcast it to others. Block is re-broadcasted only the first time when we add it to blockchain.  
                    broadcastAndUpdatePeers appState (BlockMessage block) (BlockAnswer ReceivedBlock)

    return ReceivedBlock



lightNodeHandler :: (InMemory.HasLogging appState,
    InMemory.InMemory appState m PeersSet,
    InMemory appState m (Future BlockHeader),
    InMemory appState m (Lively BlockHeader),
    AppendFixed appState m BlockHeader,
    InMemory appState m FixedLength,
    HasDB appState)
    => ForkMaxDiff -> TargetHash -> appState -> MessageHandler
lightNodeHandler forkMaxDiff targetHash =
    combineHandlers
        (const answerPing)
        (receiveBlockLight forkMaxDiff targetHash)
        (const ignoreTransaction)
        (const ignoreBlockchainQuery)
        answerContactQuery
