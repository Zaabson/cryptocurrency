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

contains :: Ord a => a -> Tree a -> Bool
contains a (Tree b ts) =
    a == b || any (contains a) ts

elems :: Tree a -> [a]
elems (Tree a ts) = a : concatMap elems ts

getElem :: Zipper a -> a
getElem (Zipper (Tree a _) _) = a

-- second one is more specific
-- prop_cyclesThroughAll :: Tree Int -> Bool
-- prop_cyclesThroughAll t = all (`elem` elements) (elems t)
--     where elements = map getElem . cycle $ toZipper t

prop_cyclesThroughAllInSingleCycle :: Tree Int -> Bool
prop_cyclesThroughAllInSingleCycle t = all (`Set.member` elemsInCycle) (elems t)
    where (x : xs) = cycle $ toZipper t
          oneCycle = x : takeWhile (\case Zipper _ Top -> False
                                          _ -> True) xs
          elemsInCycle = Set.fromList $ map getElem oneCycle

prop_finiteCycleTimeout :: Tree Int -> Property
prop_finiteCycleTimeout t = within 15000000 (uncurry (&&) . (isTop . head &&& any isTop . Prelude.tail) . cycle . toZipper $ t)
    where isTop z  = case z of Zipper _ Top -> True
                               _            -> False
