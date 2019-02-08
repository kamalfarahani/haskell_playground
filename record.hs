module RecordEx where

data Person = Person {
    name :: String,
    age :: Int
} deriving (Eq, Show)

person = Person "Kamal" 22