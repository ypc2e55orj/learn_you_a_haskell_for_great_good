{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  yourWorry <- T.getLine
  T.putStrLn $ encourage yourWorry
  main

isNegative :: Char -> Bool
isNegative = (`elem` ("非不未無" :: String))

encourage :: T.Text -> T.Text
encourage (T.uncons -> Just ((isNegative -> True), xs)) =
  T.append xs "!!"
encourage xs = xs
