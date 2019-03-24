module PhoneKeyboard where

import Data.List
import Data.Char
import Data.Maybe


type Digit = Char
type Presses = Int
type Values = String

data DaPhone = DaPhone [(Digit, Values)]

myDaphone = DaPhone 
    [('1', "1"),
     ('2', "abc"),
     ('3', "def"),
     ('4', "ghi"),
     ('5', "jkl"),
     ('6', "mno"),
     ('7', "pqrs"),
     ('8', "tuv"),
     ('9', "wxyz"),
     ('*', "^"),
     ('0', " +_"),
     ('#', "#.,")]


convo :: [String]
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol lol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone layout) char = 
    if isUpper char 
        then [('*', 1)] ++ result
        else result
    where
        result = [(digit, presses)]
        (digit, values) = head $ dropWhile f layout
            where f (_, vs) = not $ elem (toLower char) vs
        presses = idx + 1
            where Just idx = elemIndex (toLower char) values

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead daPhone text = concat $ map (reverseTaps daPhone) text

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps taps = sum $ map snd taps

groupListItemsWithCount :: Ord a => [a] -> [(a, Int)]
groupListItemsWithCount = (map (\x -> (head x, length x))).group.sort

mostFrequentListItemWithCount :: Ord a => [a] -> (a, Int)
mostFrequentListItemWithCount = 
    last.(sortBy f).groupListItemsWithCount
    where f (char1 , count1) (char2, count2) = compare count1 count2

mostPopularLetterWithCount :: String -> (Char, Int)
mostPopularLetterWithCount = mostFrequentListItemWithCount

mostPopularLetter :: String -> Char
mostPopularLetter = fst.mostPopularLetterWithCount

mostPopularLetterByCost :: String -> (Char, Int)
mostPopularLetterByCost str = 
    (letter, cost) where (letter, count) = mostPopularLetterWithCount str
                         cost = count * (fingerTaps $ reverseTaps myDaphone letter)


mostPopularWordWithCount :: String -> (String, Int)
mostPopularWordWithCount str = 
    mostFrequentListItemWithCount $ wordList
    where wordList = splitOn ' ' str

mostPopularWord :: String -> String
mostPopularWord = fst.mostPopularWordWithCount

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter.concat 

coolestWord :: [String] -> String
coolestWord = mostPopularWord.(intercalate " ") 

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn char  str = 
    chunk:(splitOn char $ drop (length chunk + 1) str)
    where chunk = takeWhile (/= char) str