{-# LANGUAGE RankNTypes  #-}

module NaturalTransformation where


type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just x) = [x]