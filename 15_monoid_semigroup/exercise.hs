module Exercise where

import Test.QuickCheck
import Data.Monoid


data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return (Identity x)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 x2) <> (Two y1 y2) = Two (x1 <> y1) (x2 <> y2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (Two x y)

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    (BoolConj b1) <> (BoolConj b2) = BoolConj (b1 || b2)

instance Monoid BoolConj where
    mempty = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = do
        b <- arbitrary
        return (BoolConj b)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj b1) <> (BoolDisj b2) = BoolDisj (b1 && b2)

instance Monoid BoolDisj where
    mempty = BoolDisj True

instance Arbitrary BoolDisj where
    arbitrary = do
        b <- arbitrary
        return (BoolDisj b)

data Or a b = 
    Fst a |
    Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Fst x) <> (Fst y) = Fst y
    (Fst x) <> (Snd y) = Snd y
    (Snd x) <> (Fst y) = Snd x
    (Snd x) <> (Snd y) = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return $ Fst x),
                    (1, return $ Snd y) ]

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    f1 <> f2 = 
        Combine (\x -> let y1 = unCombine f1 x
                           y2 = unCombine f2 x
                        in y1 <> y2)

instance Monoid b => Monoid (Combine a b) where
    mempty = Combine $ \x -> mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return (Combine f)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
    f1 <> f2 = Comp ((unComp f1) . (unComp f2))

instance Monoid (Comp a) where
    mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return (Comp f)

data Validation a b =
    Fail a |
    Succ b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Succ x) <> (Succ y) = Succ x
    (Succ x) <> (Fail y) = Succ x
    (Fail x) <> (Succ y) = Succ y
    (Fail x) <> (Fail y) = Fail (x <> y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return $ Fail x),
                    (1, return $ Succ y) ]

newtype Mem s a = Mem {
    runMem :: s -> (a, s)
}

instance Semigroup a => Semigroup (Mem s a) where
    f1 <> f2 = Mem (\s -> let (a1, s1) = (runMem f1) s
                              (a2, s2) = (runMem f2) s1
                          in  (a1 <> a2, s2))

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = 
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
                    