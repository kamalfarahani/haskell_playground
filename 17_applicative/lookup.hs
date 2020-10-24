module Lookup where

import Control.Applicative


f x = lookup x [
                  (3, "Hello"),
                  (4, "Julie"),
                  (5, "kbai")
               ]

g y = lookup y [
                  (7, "sup?"),
                  (8, "chris"),
                  (9, "aloha")
               ]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

main = do
    print $ liftA2 (++) (g 9) (f 4)
    print $ liftA2 (^) (h 5) (m 4)