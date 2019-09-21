module EitherError where

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid =  NameEmpty | AgeTooLow deriving (Eq, Show)

ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name = case name /= "" of
    True -> Right name
    False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right name) (Right age) = Right $ Person name age
mkPerson' (Left nameErrs) (Left ageErrs) = Left $ nameErrs ++ ageErrs
mkPerson' (Left nameErrs) _ = Left nameErrs
mkPerson' _ (Left ageErrs) = Left ageErrs  

