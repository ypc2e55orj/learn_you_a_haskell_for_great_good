{-# OPTIONS -Wall #-}

import System.IO

{- 9.2.1 -}
main :: IO ()
main = do
  handle <- openFile "baabaa.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
