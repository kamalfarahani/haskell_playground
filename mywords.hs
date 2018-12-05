module MyWords where

myWords :: String -> [String]

myWords "" = [] 

myWords str = [(takeWhile pred str)] ++
    (myWords $ dropWhile (not.pred) $ dropWhile pred str)
    where pred = (\ch -> ch /= ' ')