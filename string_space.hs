module StrSpace where

strToSpaceList :: String -> [String]

strToSpaceList "" = []

strToSpaceList str = [takeWhile (/= ' ') str] 
    ++ (strToSpaceList $ drop 1 $ dropWhile (/= ' ') str)


myLines :: String -> [String]

myLines "" = []

myLines str = [takeWhile (/= '\n') str] 
    ++ (myLines $ drop 1 $ dropWhile (/= '\n') str)


strToListByChar :: Char -> String -> [String]

strToListByChar _  "" = []

strToListByChar char str = [takeWhile (/= char) str]
    ++ (strToListByChar char $ drop 1 $ dropWhile (/= char) str)
