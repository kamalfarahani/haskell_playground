data TisAnInteger = TisAn Integer

data TwoIntegers = Two Integer Integer

data StringOrInt = AnInt Int | AString String

data Pair a = Pair a a

data Tuple a b = Tuple a b

data Which a = ThisOne a | ThatOne a

data EitherOr a b = Hello a | Goodbye b

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') =  x == x' && y == y'

instance Eq StringOrInt where
    (==) (AnInt x) (AnInt y) = x == y
    (==) (AString x) (AString y) =  x == y
    (==) _ _ = False

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y' 

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _ = False 