module StringProcessing where

import Data.List
import Data.Char

vowels = ['a', 'e', 'i', 'o', 'u']

replaceThe :: String -> String
replaceThe str = 
    intercalate " " (map f words) where
        words = stringToWords str
        f word = if (isThe word) 
            then "a"
            else word


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = 
    go words where
        words = stringToWords str
        go [] = 0
        go (w:[]) = 0
        go (w:ws) = 
            (go ws) + if (isThe w)
            then if isVowel((head.head) ws)
                then 1 
                else 0
            else 0

countVowels :: String -> Int
countVowels str = 
    foldr (\x acc -> if x then acc + 1 else acc) 0 list
    where list = map isVowel str

isVowel :: Char -> Bool
isVowel ch = elem (toLower ch) vowels

isThe :: String -> Bool
isThe str = 
    (str' == "the") where
        str' = map toLower str

stringToWords :: String -> [String]
stringToWords = splitOn ' '

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn char  str = 
    chunk:(splitOn char $ drop (length chunk + 1) str)
    where chunk = takeWhile (/= char) str