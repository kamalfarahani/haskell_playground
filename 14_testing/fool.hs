module Fool where

import Test.QuickCheck


data Fool =
    Fulse |
    Frue
    deriving (Eq, Show)

foolGen :: Gen Fool
foolGen =
    elements [Fulse, Frue]

foolGen' :: Gen Fool
foolGen' = 
    frequency [(2, return Frue), (1, return Fulse)]

instance Arbitrary Fool where
    arbitrary = foolGen