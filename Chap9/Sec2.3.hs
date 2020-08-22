{-# OPTIONS -Wall #-}
{- 9.2.3 -}
main :: IO ()
main = do
  c <- readFile "baabaa.txt"
  putStr c
