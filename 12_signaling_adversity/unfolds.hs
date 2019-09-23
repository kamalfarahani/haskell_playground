module Unfolds where


myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case (f x) of
    Nothing -> []
    (Just (y, x')) -> y:(myUnfoldr f x')

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = x:(myUnfoldr g x)
    where
        g x' = Just (f x', f x')