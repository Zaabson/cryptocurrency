{-# LANGUAGE LambdaCase #-}
module BlockChainTest where

import BlockChain
import Prelude hiding (cycle)
import qualified Data.Set as Set
import Test.QuickCheck
import Control.Arrow ((&&&))
import Control.Monad ( replicateM, forM )

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized $ \n ->
        if n <= 0 then
            Tree <$> arbitrary <*> return []
        else do
            let sizes = replicate n (choose (0, n `div` 2))
            let children = forM sizes (>>= (`resize` arbitrary))
            Tree <$> arbitrary <*> children

-- contains :: Eq a => a -> Tree a -> Bool
-- contains a (Tree b ts) =
--     a == b || any (contains a) ts

-- elems :: Tree a -> [a]
-- elems (Tree a ts) = a : concatMap elems ts

instance Eq a => Eq (Tree a) where
    Tree a ts == Tree b cs = a == b && and (zipWith (==) ts cs)

prop_reverseToZipper :: Tree Int -> Bool
prop_reverseToZipper t = t == fromZipper (toZipper t)


prop_pruneTrees :: Integer -> [Tree ()] -> Bool
prop_pruneTrees maxdiff ts = maxdepth - mindepth <= maxdiff
    where
        pts = prune maxdiff ts
        maxdepth = depth maximum pts
        mindepth = depth minimum pts
        depth maximum [] = 0
        depth maximum ts = 1 + maximum (map (\case Tree _ tss -> depth maximum tss) ts)
