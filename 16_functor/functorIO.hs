module FunctorIO where

getInt :: IO Int
getInt = fmap read getLine

bumpIt = fmap (+1) getInt

meTooIsm :: IO String
meTooIsm = fmap (++ " Me Too") getLine

meTooIsm' :: IO String
meTooIsm' = do
    str <- getLine
    return (str ++ " Me Too")

bumpIt' :: IO Int
bumpIt' = do
    num <- getInt
    return (num + 1)