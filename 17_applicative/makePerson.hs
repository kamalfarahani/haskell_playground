module MakePerson where

newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = fmap Address $ validateLength 100 s

mkPerson :: String -> String -> Maybe Person
mkPerson nameStr addressStr =
    Person <$> (mkName nameStr) <*> (mkAddress addressStr)