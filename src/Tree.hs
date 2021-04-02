{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Tree (Tree(..), Zipper(..), getElem, toZipper, fromZipper, modify, insertHere, dfs) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
data Tree a = Tree a [Tree a] deriving (Show, Functor, Generic) -- not empty

instance ToJSON a => ToJSON (Tree a)
instance FromJSON a => FromJSON (Tree a)

-- Focused tree.
-- Represents a location in a Tree, a node. Tree can be reconstructed from Zipper. 
--                  children ↓  , ↓ element kept at the node + "surroundings" 
data Zipper a = Zipper [Tree a] (Place a) 
    deriving Show
    -- deriving (Show, Generic)

-- instance ToJSON a => ToJSON (Zipper a)
-- instance FromJSON a => FromJSON (Zipper a)

-- element kept at the node + surroundings 
data Place a = Root a                                 -- describes a root node with element a
             | Brother (Place a) [Tree a] a [Tree a]  -- describes a children of a node described in (Place a), with element a, and given left/right siblings.
             deriving (Show, Functor)
            --  deriving (Show, Functor, Generic)

-- instance ToJSON a => ToJSON (Place a)
-- instance FromJSON a => FromJSON (Place a)

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

-- returns a list of tree's focused (zippers) on nodes in dfs order
dfs :: Tree a -> [Zipper a]
dfs t@(Tree a ts) = toZipper t : concatMap dfs ts