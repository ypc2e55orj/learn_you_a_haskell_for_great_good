{-# OPTIONS -Wall -Werror #-}
{- 5.7 -}
fn :: Double -> Integer
fn = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (< 10000) . filter odd $ map (^ (2 :: Int)) [1 ..]
