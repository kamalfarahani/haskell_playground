module ValidFunctor where

import GHC.Arr

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
    fmap f (False' x) = False' (f x)
    fmap f (True' x) = True' (f x)

data BoolAndMaybeSothingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSothingElse where
    fmap _ Falsish = Falsish
    fmap f (Truish x) = Truish (f x)


-- I couldn't figure out how to write a functor instance for this beast
newtype Mu f = InF { outF :: f (Mu f) }

data D = D (Array Word Word) Int Int

data Sum a b = First a | Second b

instance Functor (Sum a) where
    fmap _ (First x) =  First x
    fmap f (Second x) = Second $ f x

data Company a b c = DeepBlue a b | Something c

instance Functor (Company a b) where
    fmap _ (DeepBlue x y) = DeepBlue x y
    fmap f (Something x) = Something $ f x

data More a b = L b a b | R a b a

instance Functor (More a) where
    fmap f (L x y x') = L (f x) y (f x')
    fmap f (R y x y') = R y (f x) y'