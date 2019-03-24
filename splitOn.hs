module SplitOn where

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn char  str = 
    chunk:(splitOn char $ drop (length chunk + 1) str)
    where chunk = takeWhile (/= char) str