module BlockChain where

import Data.List (find)
import Tree ( Zipper(..), Tree(..), getElem, fromZipper, dfs, insertHere )
import Hashing ( shash256 ) 
import BlockType

data Blocks = Blocks { fixed :: [Block],     -- blockchain so old its not gonna change
                       lively ::  Tree Block -- newly added blocks, short forks possible
                     } 

-- Searches block tree looking for a Block that hashes to a matching previousHash,
-- returns a zipper focused on this block - the new block should be inserted here as a child 
linkToChain :: Block -> Tree Block -> Maybe (Zipper Block)
linkToChain b t =
    let prevHash = blockPreviousHash b in
    let pred = (==) prevHash . shash256 . blockHeader . getElem
    in find pred $ dfs t

insertToChain :: Block -> Tree Block -> Maybe (Tree Block)
insertToChain b t = fromZipper . insertHere b <$> linkToChain b t
