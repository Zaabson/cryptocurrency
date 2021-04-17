{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}
module BlockChain where

import Data.List (find, foldl1', foldl')
-- import Tree ( Zipper(..), Tree(..), getElem, fromZipper, dfs, insertHere )
import Hashing ( shash256 ) 
import BlockType
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Functor ( (<&>) )

-- Returns a leaf furthest from root, "arbitrary" one if there's a draw
getLastBlock :: LivelyBlocks -> Block
getLastBlock (LivelyBlocks tree) = snd $ go 0 tree
    where
        comp a@(n, _) (m, _) | n >= m = a
        comp _ b = b 
        go n (Tree b []) = (n, b)
        go n (Tree _ ts)   =  foldl1' comp . map (go (n+1)) $ ts

-- TODO: Now pruning is unnecessarily run on the whole tree at every insert. This can be improved. 

-- Forks significantly shorter (more than maxdiff shorter) than the longest one can be deleted.
prune :: Integer -> Tree a -> Tree a
prune maxdiff tree = pruneA marked

    where
        -- Mark every node with max depth among leafs in the subtree of a node  
        markWithHeight :: Integer -> Tree a -> Tree (Integer, a)
        markWithHeight d (Tree a []) = Tree (d, a) []
        markWithHeight d (Tree a ts) =
            let tt@( Tree (d0, _) _ : ts') = map (markWithHeight (d+1)) ts in 
            let maxd = foldl' (\md (Tree (dd, _) _) -> max md dd) d0 ts' in
            Tree (maxd, a) tt
        
        marked@(Tree (maxHeight, _) _) = markWithHeight 0 tree

        -- Discard tree's that don't have any leafs at depth at least equal to (maxHeight - maxdiff).
        pruneA :: Tree (Integer, a) -> Tree a
        pruneA (Tree (_, a) ts) = Tree a (map pruneA . filter (\(Tree (d, _) _) -> d >= maxHeight - maxdiff) $ ts)

newtype ForkMaxDiff = ForkMaxDiff Integer

height :: Tree a -> Integer
height (Tree _ []) = 0
height (Tree _ ts) = foldl1' max . map height $ ts

-- If the tree starting from root looks like a linked list, then split the tree into the list part and the rest.
-- The first element in the list is parent of the root of the tree.
-- Height of a tree after this operation is at least maxdiff (TODO: think of a name instead of maxdiff).  
fixBlocks :: Integer -> Tree a -> ([a], Tree a)
fixBlocks maxdiff tree = go (height tree - maxdiff) [] tree

    where
        -- recurse on the tree collecting nodes, but collecting no more than given number
        go 0 ls tree = (ls, tree)
        go n ls (Tree a [t]) = go (n-1) (a : ls) t
        go n ls tree = (ls, tree)

-- If a block can be appended to a block in LivelyBlocks, 
-- then append it, prune branches and move some of the older blocks from Lively to Fixed
updateWithBlock :: ForkMaxDiff ->  Block -> LivelyBlocks -> FixedBlocks -> (LivelyBlocks, FixedBlocks)
updateWithBlock (ForkMaxDiff maxdiff) newblock lb@(LivelyBlocks tree) fb@(FixedBlocks fixed) =
    case insertToChain newblock tree of
        Nothing    -> (lb, fb)
        Just tree' -> 
            let (newfixed, tree'') = fixBlocks maxdiff $ prune maxdiff tree'
            in (LivelyBlocks tree'', FixedBlocks (newfixed ++ fixed))
    

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

-- Finds a block in a Tree whose hash is referenced in a provided block.
-- If also provided block is not already a children of a found block, the block is inserted as a child.
-- Otherwise returns Nothing.
insertToChain :: Block -> Tree Block -> Maybe (Tree Block)
insertToChain b t =
    linkToChain b t <&> \z@(Zipper ts _) -> 
        -- insert if it's not present already
        -- TODO: could be improved with bin-search, would need to change list for vector, probably not worth
        let bHash = shash256 . blockHeader $ b in
        fromZipper $ if all (\(Tree bb _) -> shash256 (blockHeader bb) /= bHash) ts then
            insertHere b z
        else
            z

data Tree a = Tree a [Tree a] deriving (Show, Functor, Generic) -- not empty

instance ToJSON a => ToJSON (Tree a)
instance FromJSON a => FromJSON (Tree a)

-- Focused tree.
-- Represents a location in a Tree, a node. Tree can be reconstructed from Zipper. 
--                  children ↓  , ↓ element kept at the node + "surroundings" 
data Zipper a = Zipper [Tree a] (Place a) 
    deriving Show

-- element kept at the node + surroundings 
data Place a = Root a                                 -- describes a root node with element a
             | Brother (Place a) [Tree a] a [Tree a]  -- describes a children of a node described in (Place a), with element a, and given left/right siblings.
             deriving (Show, Functor)
            --  deriving (Show, Functor, Generic)

newTree :: a -> Tree a
newTree a = Tree a [] 

getElem :: Zipper a -> a
getElem (Zipper _ (Root a)) = a
getElem (Zipper _ (Brother _ _ a _)) = a

toZipper :: Tree a -> Zipper a
toZipper (Tree a ts) = Zipper ts (Root a)

fromZipper :: Zipper a -> Tree a
fromZipper (Zipper ts (Root a)) = Tree a ts
fromZipper (Zipper ts (Brother f l a r)) = fromZipper $ Zipper (reverse l ++ [Tree a ts] ++ r) f

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Zipper ts (Root a)) = Zipper ts (Root $ f a)
modify f (Zipper ts (Brother pl ls a rs)) = Zipper ts (Brother pl ls (f a) rs)

insertHere :: a -> Zipper a -> Zipper a
insertHere a (Zipper ts b) = Zipper (Tree a [] : ts) b

-- Deletes a tree rooted where we're focused on. Focus jumps to parent.
-- Returns Nothing if it's the root that is deleted and nothing is left. 
-- deleteHere :: Zipper a -> Maybe (Zipper a)
-- deleteHere (Zipper ts (Root _)) = Nothing 
-- deleteHere (Zipper ts (Brother pl ls _ rs)) = Just $ Zipper (reverse ls ++ rs) pl

-- returns a list of tree's focused (zippers) on nodes in dfs order
dfs :: Tree a -> [Zipper a]
dfs t@(Tree a ts) = toZipper t : concatMap dfs ts