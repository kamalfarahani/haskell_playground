module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
    mempty = Listy []

instance Semigroup (Listy a) where
    (Listy l1) <> (Listy l2) = Listy $ l1 <> l2