module FoldChapterEx where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (foldr (||) False) . (map f) -- myOr.(map f)

myElem :: Eq a => a -> [a] -> Bool
myElem e xs = foldr (||) False (map (== e) xs) -- myAny (==e) xs

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\a b -> [f a] ++ b) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\a b -> if f a then [a] ++ b else b) [] xs

squish :: [[a]] -> [a]
squish xs = foldr (++) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) [] --squish.(myMap f)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (xs) = foldr (\a b -> if f a b == GT then a else b) (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs

