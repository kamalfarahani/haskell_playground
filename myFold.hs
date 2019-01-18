module MyFold where

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)
myFoldr f acc [] = acc

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- foldr f z xs = foldl (flip f) z (reverse xs) (for finite lists)

myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr f acc [] = acc:[]
myScanr f acc (x:xs) = (myFoldr f acc (x:xs)):(myScanr f acc xs)

myScanl :: (b -> a -> b) -> b -> [a] -> [b]
myScanl f acc [] = acc:[]
myScanl f acc (x:xs) = acc:(myScanl f (f acc x) xs)
