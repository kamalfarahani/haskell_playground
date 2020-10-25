module ConstantExercise where

newtype Constant a b = Constant { getConstant :: a }
                       deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure x = Constant mempty
    (Constant x) <*> (Constant y) = Constant (x <> y)