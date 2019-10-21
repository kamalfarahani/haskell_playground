module ReplaceExperiment where

replaceWithP :: a -> Char
replaceWithP = const 'p'


replaceWithP' :: [Maybe[Char]] -> Char
replaceWithP' = replaceWithP


lms :: [Maybe [Char]]
lms =  [Just "Ave", Nothing, Just "woohoo"]


liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP


liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace


twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP


twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted


thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP


thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted