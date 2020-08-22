{-# OPTIONS -Wall #-}

import Control.Monad
import Data.Char

{- 9.1.1 -}
main :: IO ()
main = forever $ do
  l <- getLine
  putStrLn $ map toUpper l
