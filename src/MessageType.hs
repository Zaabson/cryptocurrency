{-# LANGUAGE DeriveGeneric #-}
module MessageType where
import Data.Aeson (ToJSON, FromJSON)
import BlockType (Block, Transaction)
import GHC.Generics (Generic)
import Server (Address)

-- Type of messages and answers send between nodes in the network. 

data Message = PingMessage
             | BlockMessage Block
             | TransactionMessage Transaction
             | BlockchainQueryMessage BlockchainQuery
             | ContactQueryMessage ContactQuery
    deriving (Show, Generic)

data BlockchainQuery
    = BlockAtHeight Integer
    deriving (Show, Generic)

data ContactQuery = ContactQuery deriving (Show, Generic)

data Answer = PingAnswer ReceivedPing
            | BlockAnswer ReceivedBlock
            | TransactionAnswer ReceivedTransaction
            | MessageParseError
            | BlockchainQueryAnswer BlockchainQueryResult
            | ContactQueryAnswer ContactQueryResult
    deriving (Show, Generic)

data ReceivedPing = ReceivedPing deriving (Show, Generic)
data ReceivedBlock = ReceivedBlock deriving (Show, Generic)
data ReceivedTransaction = ReceivedTransaction deriving (Show, Generic)

data ContactQueryResult = ContactQueryResult [Address] deriving (Show, Generic)

data BlockchainQueryResult
    = RequestedBlock Block
    | NoBlockFound
    deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

instance ToJSON BlockchainQuery
instance FromJSON BlockchainQuery

instance ToJSON ContactQuery
instance FromJSON ContactQuery

instance ToJSON Answer
instance FromJSON Answer

instance ToJSON ReceivedPing
instance FromJSON ReceivedPing

instance ToJSON ReceivedTransaction
instance FromJSON ReceivedTransaction

instance ToJSON ReceivedBlock
instance FromJSON ReceivedBlock

instance ToJSON ContactQueryResult
instance FromJSON ContactQueryResult

instance ToJSON BlockchainQueryResult
instance FromJSON BlockchainQueryResult
