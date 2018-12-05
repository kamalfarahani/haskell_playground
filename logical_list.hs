module LogicalList where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f(x) || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem element list = myAny (\x -> x == element) list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x: xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x: xs) = x ++ squish xs 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f list = squish $ map f list

squishAgain :: [[a]] -> [a]
squishAgain list = squishMap id list

myMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMaximumBy _ [] = Nothing
myMaximumBy _ [x] = Just x
myMaximumBy f (x1: x2: xs) = if (f x1 x2 == GT) 
    then myMaximumBy f (x1: xs) 
    else myMaximumBy f (x2: xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
myMinimumBy _ [] = Nothing
myMinimumBy _ [x] = Just x
myMinimumBy f (x1: x2: xs) = if (f x1 x2 == LT)
    then myMinimumBy f (x1: xs)
    else myMinimumBy f (x2: xs)