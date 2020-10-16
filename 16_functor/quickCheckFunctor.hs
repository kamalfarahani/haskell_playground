module QuickCheckFunctor where

import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == x

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => f a -> (Fun a b) -> (Fun b c) -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = ((fmap g) . (fmap f) $ x) == (fmap (g . f) x)