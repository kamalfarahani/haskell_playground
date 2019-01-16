module MyFold where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)
myFoldr f acc [] = acc

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs
