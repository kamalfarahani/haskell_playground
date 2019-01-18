module ScanEx where

factorials = scanl (*) 1 [1..]

factorial :: Int -> Integer
factorial nth = factorials !! nth