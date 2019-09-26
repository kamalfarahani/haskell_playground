module Palindrome where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char (toLower)


palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let text = prepareForPalindrome line1  
    case (text == reverse text) of
        True -> putStrLn "Its a Palindrome!"
        False -> do
            putStrLn "Not a Plaindrome -_-"
            exitSuccess
    where
        prepareForPalindrome str =
            map toLower filtered
            where
                filtered = filter (\c -> elem c badChars) str
                badChars = [' ', '\'']