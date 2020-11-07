module MonadInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg 

instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

data PhhhbbttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbttEither b) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' a)  = Left' $ f a

instance Applicative (PhhhbbttEither b) where
    pure x = Left' x
    Right' b <*> _        = Right' b
    _       <*> Right' b  = Right' b
    Left' f  <*> Left' a = Left' $ f a

instance Monad (PhhhbbttEither b) where
    return = pure
    Right' b >>= _ = Right' b
    Left' a  >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbttEither b a) where
    arbitrary =
        frequency [(1, Left' <$> arbitrary), (1, Right' <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (PhhhbbttEither a b) where
    (=-=) = eq

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil list2         = list2
append (Cons x xs) list2 = Cons x (append xs list2)

concatList :: List (List a) -> List a
concatList Nil = Nil
concatList (Cons l ls) = append l (concatList ls)

haskellListToMyList :: [a] -> (List a)
haskellListToMyList [] = Nil
haskellListToMyList (x:xs) = Cons x (haskellListToMyList xs)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    _   <*> Nil = Nil
    Nil <*> _   = Nil
    (Cons f fs)  <*> list = append (fmap f list) (fs <*> list)

instance Monad List where
    return = pure
    list >>= f = concatList $ fmap f list

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        return (haskellListToMyList x)

instance Eq a => EqProp (List a) where
    (=-=) = eq

main :: IO ()
main = do
    quickBatch $ functor (NopeDotJpg :: Nope (Int, Int, Int))
    quickBatch $ applicative (NopeDotJpg :: Nope (Int, Int, Int))
    quickBatch $ monad (NopeDotJpg :: Nope (Int, Int, Int))
    

    quickBatch $ functor (undefined :: PhhhbbttEither Int (Int, Int, Int))
    quickBatch $ applicative (undefined :: PhhhbbttEither Int (Int, Int, Int))
    quickBatch $ monad (undefined :: PhhhbbttEither Int (Int, Int, Int))
    
    quickBatch $ functor (undefined :: Identity (Int, Int, Int))
    quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
    quickBatch $ monad (undefined :: Identity (Int, Int, Int))
    
    quickBatch $ functor (undefined :: List (Int, Int, Int))
    quickBatch $ applicative (undefined :: List (Int, Int, Int))
    quickBatch $ monad (undefined :: List (Int, Int, Int))
