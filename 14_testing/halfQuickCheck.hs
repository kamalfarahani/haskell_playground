module HalfQuickCheck where

import Test.QuickCheck

half :: Float -> Float
half x = x / 2

halfIdentity :: Float -> Float
halfIdentity = (*2) . half

prop_halfIdentity :: Float -> Bool
prop_halfIdentity x = halfIdentity x == x

runQc :: IO ()
runQc = quickCheck prop_halfIdentity