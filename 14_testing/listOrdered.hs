module ListOrdered where

import Data.List (sort)
import Test.QuickCheck


listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where
        go _ status@(_, False) = status
        go x (Nothing, True) = (Just x, True)
        go y (Just x, True) = (Just y, y <= x)

prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered = listOrdered.sort

runQc :: IO ()
runQc = 
    quickCheck listSorted
    where
        listSorted :: [Integer] -> Bool
        listSorted = prop_listOrdered