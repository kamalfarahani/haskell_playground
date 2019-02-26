module StrEx where

import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf xs1 xs2 = all (flip elem $ xs2) xs1

capitalizeWords :: String -> [(String, String)]
capitalizeWords text = zip words capWords
    where words = splitOn text ' '
          capWords = map capitalizeWord words

capitalizeWord :: String -> String
capitalizeWord word = toUpper (head word) : tail word

splitOn :: String -> Char -> [String]
splitOn "" _ = [""]
splitOn (c : cs) char =
    if c == char 
        then "" : rest
        else (c : head rest) : (tail rest)
    where rest = splitOn cs char