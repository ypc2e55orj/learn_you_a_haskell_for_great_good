{-# OPTIONS -Wall #-}

import Data.Char

{- 9.1.2 -}
main :: IO ()
main = do
  contents <- getContents
  putStrLn $ map toUpper contents
  -- print contents
