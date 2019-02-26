module Cipher where

import Data.Char

caesarEncrypt :: String -> Int -> String
caesarEncrypt str shift = map shiftChar lowerStr
    where lowerStr = map toLower str 
          shiftChar ch = chr $ shifted + ord 'a' 
              where shifted = ((ord ch) - (ord 'a') + shift) `mod` 26

caesarDecrypt :: String -> Int -> String
caesarDecrypt str shift = caesarEncrypt str (-shift)

vigenereEncrypt :: String -> String -> String
vigenereEncrypt "" _ = ""
vigenereEncrypt str keyword = go str (repeatKeyword str keyword) where
    go "" _ = ""
    go str' repeatedKeyword =
        if head str' == ' ' 
            then " " ++ go (tail str') (tail repeatedKeyword)
            else  [shiftLetter (head str') shift] ++ go (tail str') (tail repeatedKeyword)
            where shift = (ord $ toLower $ head repeatedKeyword) - ord 'a'

shiftLetter :: Char -> Int -> Char
shiftLetter ch shift = 
    chr $ (((ord lowerChar) - aASCICode + shift) `mod` alphabetNumber) + aASCICode
    where lowerChar = toLower ch
          alphabetNumber = 26
          aASCICode = ord 'a'
          

repeatKeyword :: String -> String -> String
repeatKeyword str keyword = go str keyword where
                go "" _ = ""
                go str' "" = go str' keyword
                go str' keyword' = 
                    if head str' == ' ' 
                        then " " ++ go (tail str') keyword' 
                        else [head keyword'] ++ go (tail str') (tail keyword')
