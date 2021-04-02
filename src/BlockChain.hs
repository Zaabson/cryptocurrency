{-# LANGUAGE DeriveGeneric #-}
module BlockChain where

import Data.List (find)
import Tree ( Zipper(..), Tree(..), getElem, fromZipper, dfs, insertHere )
import Hashing ( shash256 ) 
import BlockType
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- we will be appending new blocks, so let the order be:
-- head fixedBlocks == newest block
-- last fixedBlocks == first block after genesis
newtype FixedBlocks = FixedBlocks {getFixedBlocks :: [Block]}
    deriving (Show, Generic)
newtype LivelyBlocks = LivelyBlocks { getLivelyBlocks :: Tree Block}
    deriving (Show, Generic)

instance ToJSON FixedBlocks
instance FromJSON FixedBlocks

instance ToJSON LivelyBlocks
instance FromJSON LivelyBlocks

-- type not in use but this is blockchain: 
data Blockchain = Blockchain FixedBlocks LivelyBlocks Genesis


-- Searches block tree looking for a Block that hashes to a matching previousHash,
-- returns a zipper focused on this block - the new block should be inserted here as a child 
linkToChain :: Block -> Tree Block -> Maybe (Zipper Block)
linkToChain b t =
    let prevHash = blockPreviousHash b in
    let pred = (==) prevHash . shash256 . Right . blockHeader . getElem
    in find pred $ dfs t

insertToChain :: Block -> Tree Block -> Maybe (Tree Block)
insertToChain b t = fromZipper . insertHere b <$> linkToChain b t
