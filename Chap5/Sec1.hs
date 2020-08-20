{-# OPTIONS -Wall -Werror #-}
{- 5.1 -}
compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = flip (elem) ['A' .. 'Z']
