module Sum where

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second x) = Second (f x)
    fmap f (First x) = First x