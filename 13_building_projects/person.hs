module Person where


type Name = String
type Age = Integer

data Person = 
    Person Name Age
    deriving (Show, Eq)

data PersonInvalid =
    NameEmpty |
    AgeTooLow |
    PersonInvalidUnknown String
    deriving (Show, Eq)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown "Unknown Err"

gimmePerson :: IO ()
gimmePerson = do
    putStr "Enter name: "
    name <- getLine
    putStr "Enter age: "
    ageStr <- getLine
    
    let personOrErr = mkPerson name (read ageStr)
    case personOrErr of
        Left err -> putStrLn $ show err
        Right person ->
            do
                putStrLn "Yay Person created!!"
                putStrLn $ show person


    