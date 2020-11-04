module DoSyntax where

import Control.Applicative ((*>))


sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another blah"

sequencing' :: IO ()
sequencing' = 
    putStrLn "blah" >>
    putStrLn "another blah"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' =
    getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "Name Pls: "
    name <- getLine
    putStrLn ("Hello " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "Name Pls: " >>
    getLine >>=
    \name ->
        putStrLn ("Hello " ++ name)

twoBinds :: IO ()
twoBinds = do
    putStrLn "Name Pls: "
    name <- getLine
    putStrLn "Age Pls: "
    age <- getLine

    putStrLn ("Name: " ++ name ++ " And Age: " ++ age)

twoBinds' :: IO ()
twoBinds' = 
    putStrLn "Name Pls: " >>
    getLine >>=
    \name ->
    putStrLn "Age Pls:" >>
    getLine >>=
    \age ->
    putStrLn ("Name: " ++ name ++ " And Age: " ++ age)
    