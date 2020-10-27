module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

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

instance Eq a => EqProp (List a) where
    (=-=) = eq

main :: IO ()
main =
    quickBatch (applicative (Cons ("", "", "") Nil))