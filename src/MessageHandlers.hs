{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module MessageHandlers where

import Data.Aeson ( FromJSON, ToJSON, decode, encode )
import BlockType (Block, Transaction, blockBlockHeight)
import Server (Address(Address), ServerHandler)
import qualified Data.Sequence as Seq (Seq, (|>))
import InMemory (HasLogging (logger), InMemoryRead (readMemoryIO), InMemory (modifyMemory), MonadAtomic (runAtomically))
import BlockValidation (UTXOPool, validTransaction)
import BlockChain (Fixed(Fixed), FixedBlocks, ForkMaxDiff)
import Data.List (find)
import Hashing (TargetHash)
import Node (getAddresses, PeersSet)
import MessageType
    ( BlockchainQueryResult(..),
      ContactQueryResult(..),
      ReceivedTransaction(..),
      ReceivedBlock,
      ReceivedPing (ReceivedPing),
      Answer(..),
      ContactQuery,
      BlockchainQuery(..),
      Message(..) )
import qualified Data.Set as Set

newtype MsgHandler msg answer = MsgHandler (msg -> IO answer)

type MessageHandler = MsgHandler Message Answer

combineHandlers :: forall appState.
       (appState -> MsgHandler () ReceivedPing)  -- react to PingMessage
    -> (appState -> MsgHandler Block ReceivedBlock) -- react to BlockMessage
    -> (appState -> MsgHandler Transaction ReceivedTransaction)
    -> (appState -> MsgHandler BlockchainQuery BlockchainQueryResult)
    -> (appState -> MsgHandler ContactQuery ContactQueryResult)
    -> appState -> MessageHandler
combineHandlers pingH blockH txH chainH contactH appState = MsgHandler msgHandler
    where
        msgHandler PingMessage                    = PingAnswer            <$> call pingH ()
        msgHandler (BlockMessage block)           = BlockAnswer           <$> call blockH block
        msgHandler (TransactionMessage tx)        = TransactionAnswer     <$> call txH tx
        msgHandler (BlockchainQueryMessage query) = BlockchainQueryAnswer <$> call chainH query
        msgHandler (ContactQueryMessage query)    = ContactQueryAnswer    <$> call contactH query

        call :: (appState -> MsgHandler a b) -> a -> IO b
        call f = let MsgHandler h = f appState in h


toServerHandler :: MessageHandler
                -> (String -> IO ())  -- what to do on parsing error, (String -> IO() / IO())
                -> ServerHandler
toServerHandler (MsgHandler handler) log _ msgbytes = do
    case decode msgbytes of
        Nothing -> do
            log "handler: Unable to parse message."
            return $ encode MessageParseError
        Just msg -> do
            answer <-  handler msg
            return $ encode answer


newtype TransactionQueue = TransactionQueue {
    getTransactionQueue :: Set.Set Transaction
}

newIncomingTransaction :: TransactionQueue -> Transaction -> TransactionQueue
newIncomingTransaction (TransactionQueue s) tx = TransactionQueue (tx `Set.insert` s)

removeUsedTransactions ::  TransactionQueue -> Set.Set Transaction -> TransactionQueue
removeUsedTransactions (TransactionQueue s) remove = TransactionQueue (Set.difference s remove)


receiveTransaction :: (HasLogging appState, InMemoryRead appState UTXOPool, InMemory appState m TransactionQueue) =>
    appState -> MsgHandler Transaction ReceivedTransaction
receiveTransaction appState = MsgHandler $ \tx -> do
    -- append new transaction to queue
    -- Transactions are initially validated against utxo's from known FixedBlocks.
    -- TODO: This (maybe) should be updated to validate for state at the tip of LivelyBlocks.

    utxoPool <- readMemoryIO appState
    if validTransaction utxoPool tx then do
        logger appState "handler: Received new transaction."
        runAtomically $ modifyMemory appState (`newIncomingTransaction` tx)
    else
        logger appState "handler: Received new transaction."

    return ReceivedTransaction

ignoreTransaction :: MsgHandler Transaction ReceivedTransaction
ignoreTransaction = MsgHandler $ const (return ReceivedTransaction)


-- For full node.
answerBlockchainQuery :: (HasLogging appState, InMemoryRead appState FixedBlocks) =>
    appState -> MsgHandler BlockchainQuery BlockchainQueryResult
answerBlockchainQuery appState = MsgHandler $ \query -> do
    logger appState "handler: Received a blockchain query."
    case query of
        -- This is pretty tragic as is indexing in a linked list
        BlockAtHeight n -> do
            blocks <- readMemoryIO appState
            case blocks of
                Fixed [] -> return NoBlockFound
                Fixed (b : bs) ->
                    if blockBlockHeight b >= n then
                        case find (\x -> blockBlockHeight x == n) (b:bs) of
                            Just b -> do return (RequestedBlock b)
                            Nothing -> return NoBlockFound
                    else
                        return NoBlockFound

ignoreBlockchainQuery :: MsgHandler a BlockchainQueryResult
ignoreBlockchainQuery = MsgHandler $ const (return NoBlockFound)

-- genericHandleBlock - actually there is shared between light and full node handling of a block

answerContactQuery :: (HasLogging appState, InMemoryRead appState PeersSet)  => appState -> MsgHandler ContactQuery ContactQueryResult
answerContactQuery appState = MsgHandler $ \query -> do
    logger appState "handler: Received a contact query."
    peers <- readMemoryIO appState
    return (ContactQueryResult $ getAddresses peers)


answerPing :: MsgHandler () ReceivedPing
answerPing = MsgHandler $ const (return ReceivedPing)
