module ValidationApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Validation e a = Error e | OK a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Error x) = Error x
    fmap f (OK x)    = OK (f x)

instance Monoid e => Applicative (Validation e) where
    pure = OK
    (OK f)       <*> (OK y)       = OK (f y)
    (Error err)  <*> (OK y)       = Error err
    (OK f)       <*> (Error err)  = Error err
    (Error err1) <*> (Error err2) = Error (err1 <> err2)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        frequency [(1, return (Error e)), (1, return (OK a))]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

main :: IO ()
main = 
    quickBatch $ applicative (OK ("", "", "") :: Validation String (String, String, String))