module FiboScan where


fibs = 1:scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN nth = fibs !! nth

fibsLessThan :: Integer -> [Integer]
fibsLessThan num = takeWhile (< num) fibs