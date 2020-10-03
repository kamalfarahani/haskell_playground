module Idempotence where

import Test.QuickCheck 
import Data.Char (toUpper)
import Data.List (sort)


capitalizeWord :: String -> String
capitalizeWord = fmap toUpper

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

f :: String -> Bool
f x = 
    (capitalizeWord x == twice capitalizeWord x)
    &&
    (capitalizeWord x == fourTimes capitalizeWord x)

f' :: Ord a => [a] -> Bool
f' x = 
    (sort x == twice sort x)
    &&
    (sort x == fourTimes sort x)

runQC :: IO ()
runQC = do
    quickCheck f
    quickCheck (f' :: [Integer] -> Bool)