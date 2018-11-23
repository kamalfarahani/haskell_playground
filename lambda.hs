module FunctionWithLambda where

printPlusTwo n = (\plusTwo -> print plusTwo)(n+2)