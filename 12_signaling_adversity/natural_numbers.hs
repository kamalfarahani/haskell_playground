module NaturalNumbers where

data Nat =
    Zero |
    Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat int
    | int < 0 = Nothing
    | int == 0 = Just Zero
    | otherwise = Just $ Succ $ perivousNat 
        where
            Just perivousNat = integerToNat (int - 1)