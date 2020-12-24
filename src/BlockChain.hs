module BlockChain where

import Data.List (find)
import Zipper
import Hashing ( shash256 ) 
import BlockType

data Blocks = Blocks { fixed :: [Block],     -- blockchain so old its not gonna change
                       lively ::  Tree Block -- newly added blocks, small forks possible
                     } 

-- Searches block tree looking for a Block that hashes to a matching previousHash,
-- returns a zipper focused on this block - the new block should be inserted here as a child 
linkToChain :: Block -> Tree Block -> Maybe (Zipper Block)
linkToChain b t =
    let prevHash = blockPreviousHash b in
    let pred = (==) prevHash . shash256 . blockHeader . getElem
    in find pred $ fullCycle t

insertToChain :: Block -> Tree Block -> Maybe (Tree Block)
insertToChain b t = do
    Zipper ts p <- linkToChain b t
    return $ fromZipper $ Zipper (Tree b [] : ts) p
