 {-# LANGUAGE FlexibleInstances #-}

module FunctorInstances where


data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
    fmap f (Bloor x) = Bloor $ f x
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K x) = K x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K x)) = Flip (K (f x))

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst x) = GoatyConst (f x)

data LiftItOut f a = LiftItOut (f a) deriving (Eq ,Show)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut x) = LiftItOut (fmap f x)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious x y z) = Notorious x y (fmap f z)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap f (Cons x list) = Cons (f x) (fmap f list) 
    fmap _ Nil = Nil

data GoatLord a = NoGoat |
                  OneGoat a |
                  MoreGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)
                  deriving (Eq, Show)

instance Functor GoatLord where
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats g1 g2 g3) = MoreGoats (f <$> g1) (f <$> g2) (f <$> g3)
    fmap _ NoGoat = NoGoat

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap f (Print str x) = Print str (f x)
    fmap f (Read strToX) = Read (fmap f strToX) 
    fmap _ Halt = Halt 