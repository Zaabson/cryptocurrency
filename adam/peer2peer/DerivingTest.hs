{-# LANGUAGE DeriveGeneric #-}
module DerivingTest where

import GHC.Generics
import Data.Aeson

data Foo = Foo {
    a :: [Int],
    b :: String
    } deriving (Generic)

