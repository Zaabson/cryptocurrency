module ClientTest where

import Client

prop_split_reverses_append :: [String] -> Bool
prop_split_reverses_append xs = xs == splitMessages (concatMap appendLenBits xs)