module Main where

import Data.Monoid
import Test.QuickCheck


data Bull = 
    Fools |
    Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty = Fools
    
data First'  a =
    First' { getFirst' :: Maybe a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        x <- arbitrary
        return (First' x)

instance Semigroup (First' a) where
    (First' (Just a)) <> _ = First' (Just a)
    (First' Nothing) <> (First' (Just b)) = First' (Just b)
    (First' Nothing) <> (First' Nothing) = First' Nothing

instance Monoid (First' a) where
    mempty = First' Nothing

type BullMappend = Bull -> Bull -> Bull -> Bool
type FirstMappend = (First' String) -> (First' String) -> (First' String) -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
    putStrLn "Testing Bull: "
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)
    putStrLn "Now For First': "
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: First' String -> Bool)
    quickCheck (monoidRightIdentity :: First' String -> Bool)
