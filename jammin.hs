module Jammin where

import Data.List

data Fruit = Peach 
           | Pulm
           | Apple
           | Blackberry
           deriving (Eq, Show, Ord)

data JamJars = Jam {
    fruit :: Fruit,
    jars :: Int
} deriving (Eq, Show, Ord)

compareJams :: JamJars -> JamJars -> Ordering
compareJams (Jam k _) (Jam k' _) = compare k k'

compareJars :: JamJars -> JamJars -> Ordering
compareJars (Jam _ j) (Jam _ j') = compare j j'

totalNumberOfJars :: [JamJars] -> Int
totalNumberOfJars jamJars = foldr (+) 0 (map jars jamJars)

mostRow :: [JamJars] -> JamJars
mostRow = maximumBy compareJars

sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy compareJams

groupJam :: [JamJars] -> [[JamJars]]
groupJam = (groupBy isSameKind).sortJams 
           where isSameKind j1 j2 = (compareJams j1 j2) == EQ