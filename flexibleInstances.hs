{-# Language FlexibleInstances #-}

module FlexibleInstancesEx where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany num = num > 10000

--Here flexible instances is used
instance TooMany String where
    tooMany str = length str > 100

instance TooMany (Int, String) where
    tooMany (int, str) = tooMany int && tooMany str

instance TooMany (Int, Int) where
    tooMany (int1, int2) = tooMany (int1 + int2)

instance (Num a, TooMany a, Ord a) => TooMany (a, a) where
    tooMany (x, y) = x + y < 100000