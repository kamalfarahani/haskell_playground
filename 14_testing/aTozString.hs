module AToZString where

import Test.QuickCheck

genAToZString :: Gen String
genAToZString = do
    x <- arbitrary
    let s :: String
        s = if x > 0 then aTozStrings !! x else ""
    return s
    where
        aTozStrings = [c:s | s <- "":aTozStrings, c <- ['a'..'z'] ++ ['A'..'Z']]