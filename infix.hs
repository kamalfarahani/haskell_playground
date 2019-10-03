module Infix where

-- infix by default
(<<||>>) x y = x + y

-- infix by default
data Q = String :!!: Int

three = 1 <<||>> 3
q = "S" :!!: 3