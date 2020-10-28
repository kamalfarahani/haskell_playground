module ZipListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' num (Cons x xs) =
    if num <= 0
        then Nil
        else (Cons x (take' (num - 1) xs))

append :: (List a) -> (List a) -> (List a)
append l1 Nil = l1
append Nil l2 = l2
append (Cons x xs) l2 = Cons x (append xs l2)

fold :: (a -> b -> b) -> b -> (List a) -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> (List a)
concat' = fold append Nil

flatMap :: (a -> List b) -> (List a) -> (List b)
flatMap f (Cons x xs) = append (f x) (flatMap f xs)
flatMap f Nil = Nil

haskellListToMyList :: [a] -> (List a)
haskellListToMyList [] = Nil
haskellListToMyList (x:xs) = Cons x (haskellListToMyList xs)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    fList <*> list = flatMap (\f -> fmap f list) fList

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        return (haskellListToMyList x)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure x = 
        ZipList' (infinite x)
        where
            infinite x = Cons x (infinite x)
    
    _                      <*> (ZipList' Nil)         = ZipList' Nil
    (ZipList' Nil)         <*>  _                     = ZipList' Nil
    (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) = 
        ZipList' (Cons (f x) listApplied)
        where
            ZipList' listApplied = ZipList' fs <*> ZipList' xs

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = do
        x <- arbitrary
        return (ZipList' x)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
       where xs' = let (ZipList' l) = xs
                   in take' 3000 l
             ys' = let (ZipList' l) = ys
                   in take' 3000 l

main :: IO ()
main = quickBatch $ applicative (ZipList' (Cons ("", "", "") Nil))