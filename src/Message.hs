{-# LANGUAGE DeriveGeneric #-}
module Message where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import BlockType (Block, Transaction)
import GHC.Generics (Generic)

data Message = PingMessage
             | BlockMessage Block
             | TransactionMessage Transaction
    deriving (Show, Generic)

data Answer = AnswerPing
            | ReceivedBlock
            | ReceivedTransaction
            | MessageParseError
    deriving (Show, Generic)

instance ToJSON Message
instance FromJSON Message

instance ToJSON Answer
instance FromJSON Answer

-- honestly, do I need to write these myself? 
    
-- instance FromJSON Message where
--     parseJSON = withObject "Message" $ \o -> do
--         msgtype :: String <- o .: "messageType"
--         case msgtype of
--             "ping"        -> return PingMessage
--             "block"       -> BlockMessage <$> parseJSON (Object o)
--             "transaction" -> TransactionMessage <$> parseJSON (Object o)

-- instance ToJSON Message where
--     -- note: should be String "ping" :: Value, but Value is instance of IsString what OverloadedStrings pragma make use of
--     toJSON PingMessage             = object [("messageType", "ping")]
--     toJSON (BlockMessage block)    = Object $ 
--         toObject block <>
--         HM.singleton "messageType" "block"
--     toJSON (TransactionMessage tx) = Object $
--         toObject tx <>
--         HM.singleton "messageType" "transaction"

-- toObject :: ToJSON a => a -> Object
-- toObject a = case toJSON a of
--     Object o -> o
--     _        -> error "toObject: value isn't an Object"
