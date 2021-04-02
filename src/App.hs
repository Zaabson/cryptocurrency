{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module App where

import Server (Address)
import BlockType (Block, Transaction)
import System.IO (withBinaryFile, IOMode (ReadMode))
import Data.Aeson (encode, decode, encodeFile, FromJSON, ToJSON)
import Data.ByteString.Lazy (hGetContents)
import Control.Concurrent.STM (TVar, STM, modifyTVar', stateTVar, newTVarIO, readTVarIO)
import BlockChain (LivelyBlocks, FixedBlocks (FixedBlocks))
import qualified Data.Sequence as Seq (Seq, (<|), take, length, splitAt)
import Control.Arrow (first)
import GHC.Generics (Generic)


data MiningMode
    = Mining
    | Idle

loadBlockchain :: FilePath -> IO (Maybe FixedBlocks)
loadBlockchain path = withBinaryFile path ReadMode ((decode <$>) . hGetContents) 

-- saveBlockChain == Data.Aeson.encodeFile

data Save = Save LivelyBlocks (Seq.Seq Transaction)
    deriving (Show, Generic)

instance ToJSON Save 
instance FromJSON Save

-- probably will be used for manual testing
--             Fixedblocks path, Save path
loadSavedState :: FilePath -> FilePath -> IO (Maybe AppState)
loadSavedState fixedpath savepath = do 
    mfixed <- loadBlockchain fixedpath
    msave <- withBinaryFile savepath ReadMode ((decode <$>) . hGetContents)
    case (mfixed, msave) of
        (Just fixed, Just (Save lively txs)) -> do
            recentBlocks <- newTVarIO lively
            oldBlocks    <- newTVarIO fixed
            incomingTxs  <- newTVarIO txs
            return $ Just $ AppState {recentBlocks = recentBlocks, 
                                      oldBlocks    = oldBlocks,
                                      incomingTxs  = incomingTxs }
        _ -> return Nothing

saveAppState :: FilePath -> FilePath -> AppState -> IO ()
saveAppState fixedpath savepath AppState {..} = do 
    lively <- readTVarIO recentBlocks
    fixed <- readTVarIO oldBlocks
    txs <- readTVarIO incomingTxs
    encodeFile fixedpath fixed
    encodeFile savepath $ Save lively txs


data AppState = AppState {
    recentBlocks :: TVar LivelyBlocks,
    oldBlocks    :: TVar FixedBlocks,
    incomingTxs  :: TVar (Seq.Seq Transaction)
    }

appendTransaction :: Transaction -> AppState -> STM ()
appendTransaction txs AppState {..} = modifyTVar' incomingTxs (txs Seq.<|)

-- Take n transaction or fail if there is less
takeNTransactions :: Int -> AppState -> STM (Maybe (Seq.Seq Transaction))
takeNTransactions n AppState {..} = stateTVar incomingTxs $ \txs ->
    if Seq.length txs >= n then
        first Just $ Seq.splitAt n txs
    else
        (Nothing, txs)

