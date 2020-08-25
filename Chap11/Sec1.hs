{-# OPTIONS -Wall #-}

import Data.Char
import Data.List

{- 11.1 -}
main :: IO ()
main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
