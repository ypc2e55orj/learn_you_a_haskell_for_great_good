{-# OPTIONS -Wall #-}

import Data.Char
import Data.List

{- 10.1.1 -}
solveRPN :: String -> Double
solveRPN = head . foldl' solve [] . words
  where
    solve (r : l : ns) "*" = (l * r) : ns
    solve (r : l : ns) "/" = (l / r) : ns
    solve (r : l : ns) "+" = (l + r) : ns
    solve (r : l : ns) "-" = (l - r) : ns
    solve (r : l : ns) "^" = (l ** r) : ns
    solve (n : ns) "log" = log n : ns
    solve ns "sum" = [sum ns]
    solve ns n =
      let isDouble = foldr (\x a -> if isDigit x || x == '.' then a else False) True
       in if isDouble n
            then (read n :: Double) : ns
            else error "Detect a unknown token."
