module Kamal where

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance Functor (Or a) where
    fmap _ (First x) = First x
    fmap f (Second x) = Second $ f x