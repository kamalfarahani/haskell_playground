module Cipher where

import Data.Char

caesarEncrypt :: String -> Int -> String

caesarEncrypt str shift = map shiftChar lowerStr
    where lowerStr = map toLower str 
          shiftChar ch = chr $ shifted + ord 'a' 
              where shifted = ((ord ch) - (ord 'a') + shift) `mod` 26


caesarDecrypt :: String -> Int -> String

caesarDecrypt str shift = caesarEncrypt str (-shift)