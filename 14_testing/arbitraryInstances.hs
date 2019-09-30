module ArbitraryInstances where

import Test.QuickCheck


data Trivial =
    Trivial
    deriving (Eq, Show)

data Identity a =
    Identity a
    deriving (Eq, Show)

data Pair a b =
    Pair a b
    deriving (Eq, Show)

data Sum a b = 
    First a |
    Second b
    deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    x <- arbitrary
    return (Identity x)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    x <- arbitrary
    y <- arbitrary
    return (Pair x y)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
    x <- arbitrary
    y <- arbitrary
    oneof [return $ First x,
           return $ Second y]

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
    x <- arbitrary
    y <- arbitrary
    frequency [(10, return $ First x),
               (1, return $ Second y)]

instance Arbitrary Trivial where
    arbitrary = trivialGen

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = sumGenEqual