module BadMonoid where

import Data.Monoid
import Test.QuickCheck

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty = Fools

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
    quickCheck (monoidAssoc :: Bull -> Bull -> Bull -> Bool)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)