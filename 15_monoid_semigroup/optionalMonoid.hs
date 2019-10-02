module OptionalMonoid where

import Data.Monoid


data Optional a = 
    Nada |
    Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    (Only a) <> (Only b) = Only (a <> b)
    (Only a) <> Nada = Only a
    Nada <> (Only a) = Only a
    Nada <> Nada = Nada