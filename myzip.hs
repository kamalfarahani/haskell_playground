module MyZip where

myZip :: [a] -> [b] -> [(a, b)]

myZip l1 l2 = myZipWith (,) l1 l2


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith _ _ [] = []

myZipWith _ [] _ = []

myZipWith f l1 l2 = [f (head l1) (head l2)] ++
    myZipWith f (tail l1) (tail l2) 