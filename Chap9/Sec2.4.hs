{-# OPTIONS -Wall #-}

import Data.Char

{- 9.2.4 -}
main :: IO ()
main = do
  c <- readFile "baabaa.txt"
  writeFile "baabaacaps.txt" (map toUpper c)
