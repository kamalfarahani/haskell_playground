{-# Language GeneralizedNewtypeDeriving #-}
module LGD where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 1000

newtype Goats = Goats Int deriving (Show, Eq, TooMany)
