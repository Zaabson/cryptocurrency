module BlockChainTest where

import BlockChain
import Prelude hiding (cycle)
import Test.QuickCheck
import Control.Monad ( replicateM, forM ) 

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized $ \n ->
        if n <= 0 then
            Tree <$> arbitrary <*> return [] 
        else do
            let sizes = replicate n (choose (0, n `div` 2))
            let children = forM sizes (>>= (`resize` arbitrary))
            Tree <$> arbitrary <*> children

contains :: Ord a => a -> Tree a -> Bool 
contains a (Tree b ts) =
    a == b || any (contains a) ts

elems :: Tree a -> [a]
elems (Tree a ts) = a : concatMap elems ts

prop_cyclesThroughAll :: Tree Int -> Bool
prop_cyclesThroughAll t = all (`elem` c) (elems t)
    where c = map fromZipper . cycle $ toZipper t
          fromZipper (Zipper (Tree a _) _) = a