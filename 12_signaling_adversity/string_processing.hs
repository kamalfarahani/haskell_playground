module StringProcessing where
import Data.Char
import Data.List

newtype Word' = 
    Word' String
    deriving (Eq, Show)

vowels = ['a', 'e', 'i', 'o', 'u']

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe word = Just word

replaceThe :: String -> String
replaceThe str =
    intercalate " " (map replaceTheWithA (words str))
    where
        replaceTheWithA x =
            if ((map toLower x) == "the") then "a" else x

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str =
    go (words str)
    where
        go (w:ws)
            | w == "the" && (startsWithVowel $ head ws) = 1 + go ws
            | ws == [] = 0
            | otherwise = 0 + go ws

startsWithVowel :: String -> Bool
startsWithVowel str = any (\vowel ->  toLower (head str) == vowel) vowels

countVowels :: String -> Integer
countVowels str =
    foldr addOneIfIsVowel 0 str
    where
        addOneIfIsVowel =
            (\ch acc -> if  (isVowel ch) then acc + 1 else acc)

isVowel :: Char -> Bool
isVowel ch = elem (toLower ch) vowels

mkWord :: String -> Maybe Word'
mkWord str =
    if numberOfVowels > numberOfConsonants
        then Nothing
        else Just $ Word' str
    where
        numberOfVowels = countVowels str
        numberOfConsonants = (toInteger $ length str) - numberOfVowels