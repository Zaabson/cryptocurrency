module Zipper where

import Prelude hiding (Left, Right, cycle)
import GHC.Base ( Alternative((<|>)) )
import Data.Functor ( (<&>) )
import Data.Maybe (fromMaybe)

data Tree a = Tree a [Tree a] deriving Show -- not empty

-- Represents a location in a Tree, a node. Tree can be reconstructed from Zipper. 
--                  children ↓  , ↓ element kept at the node + "surroundings" 
data Zipper a = Zipper [Tree a] (Place a) deriving Show

-- element kept at the node + surroundings 
data Place a = Root a                                 -- describes a root node with element a
             | Brother (Place a) [Tree a] a [Tree a]  -- describes a children of a node described in (Place a), with element a, and given left/right siblings.
             deriving Show

getElem :: Zipper a -> a
getElem (Zipper _ (Root a)) = a
getElem (Zipper _ (Brother _ _ a _)) = a

toZipper :: Tree a -> Zipper a
toZipper (Tree a ts) = Zipper ts (Root a)

fromZipper :: Zipper a -> Tree a
fromZipper (Zipper ts (Root a)) = Tree a ts
fromZipper (Zipper ts (Brother f l a r)) = fromZipper $ Zipper (reverse l ++ [Tree a ts] ++ r) f

data Dir = Down | Up | Left | Right

goDown :: Zipper a -> Maybe (Zipper a)
goDown (Zipper [] _) = Nothing
goDown (Zipper ((Tree a ts) : cs) p) = Just $ Zipper ts (Brother p [] a cs)

goUp :: Zipper a -> Maybe (Zipper a)
goUp (Zipper _ (Root _)) = Nothing
goUp (Zipper ts (Brother father l a r)) = Just $ Zipper (reverse l ++ [Tree a ts] ++ r) father

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Zipper _ (Root _)) = Nothing 
goRight (Zipper _ (Brother _ _ _ [])) = Nothing
goRight (Zipper ts (Brother f l a (Tree e cs : rs))) = Just $ Zipper cs (Brother f (Tree a ts : l) e rs)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Zipper _ (Root _)) = Nothing 
goLeft (Zipper _ (Brother _ [] _ _)) = Nothing
goLeft (Zipper ts (Brother f (Tree e cs : ls) a r)) = Just $ Zipper cs (Brother f ls e (Tree a ts : r))

-- Cycles through a tree in a zipper form, visiting some nodes few times
cycle :: Zipper a -> [Zipper a]
cycle zip = zip : nexts zip Down
    where 
        nexts zip Down = 
            let down = goDown zip <&> \x -> x : nexts x Right
                left = goLeft zip <&> \x -> x : nexts x Down
                up   = goUp zip   <&> \x -> x : nexts x Left 
            -- if can't go up, down, and left it means zip is Root with no children 
            in fromMaybe (repeat zip) (down <|> left <|> up)
        nexts zip Right =
            case goRight zip of
                Nothing -> nexts zip Down    -- we hit the end 
                Just x  -> x : nexts x Right -- go til we hit the end
        nexts zip Up =
            case goUp zip of
                Nothing -> nexts zip Down
                Just x  -> x : nexts x Left
        nexts zip Left =
            case goLeft zip of
                Nothing -> nexts zip Up 
                Just x  -> x : nexts x Down

fullCycle :: Tree a -> [Zipper a]
fullCycle t = z : takeWhile (not . isRoot) zs
    where isRoot z  = case z of Zipper _ (Root _) -> True
                                _             -> False
          (z : zs) = cycle $ toZipper t
