module StrEx where

import Data.Char
import Data.List

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf xs1 xs2 = all (flip elem $ xs2) xs1

capitalizeWords :: String -> [(String, String)]
capitalizeWords text = zip words capWords
    where words = splitOn text ' '
          capWords = map capitalizeWord words

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph text = intercalate "." capParagraphs
    where paragraphs = splitOn text '.'
          capParagraphs = map (capFirstWord) paragraphs
          capFirstWord paragraph = firstSpaces ++ (capitalizeWord firstWord) ++ rest
              where firstWord = takeWhile (/= ' ') $ drop (length firstSpaces) paragraph
                    firstSpaces = takeWhile (==' ') paragraph
                    rest = drop (length firstWord + length firstSpaces) paragraph
          

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord word = toUpper (head word) : tail word

splitOn :: String -> Char -> [String]
splitOn "" _ = [""]
splitOn (c : cs) char =
    if c == char 
        then "" : rest
        else (c : head rest) : (tail rest)
    where rest = splitOn cs char