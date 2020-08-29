{-# OPTIONS -Wall #-}

module Main where

import Control.Monad

{- 14.6 -}
-- >>> readMaybe "1" :: Maybe Int
-- Just 1
-- >>> readMaybe "hoge" :: Maybe Int
-- Nothing
readMaybe :: Read a => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

-- >>> folding [3, 2] "*"
-- Just [6.0]
-- >>> folding [3, 2] "-"
-- Just [-1.0]
-- >>> folding [] "*"
-- Nothing
-- >>> folding [] "1"
-- Just [1.0]
folding :: [Double] -> String -> Maybe [Double]
folding (x : y : ys) "*" = return ((y * x) : ys)
folding (x : y : ys) "+" = return ((y + x) : ys)
folding (x : y : ys) "-" = return ((y - x) : ys)
folding xs numberString = liftM (: xs) (readMaybe numberString)

-- >>> solveRPN "1 2 * 4 +"
-- Just 6.0
-- >>> solveRPN "1 2 * 4 + 5 *"
-- Just 30.0
-- >>> solveRPN "1 2 * 4"
-- Nothing
-- >>> solveRPN "1 2 hoge"
-- Nothing
solveRPN :: String -> Maybe Double
solveRPN st = do
  [res] <- foldM folding [] (words st)
  return res
