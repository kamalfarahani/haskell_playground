module NewTypeExample where

newtype Goats = Goats Int deriving (Eq, Show)

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 10000

-- We can implement TooMany Type class seperate for Goats
instance TooMany Goats where 
    tooMany (Goats n) = n > 42