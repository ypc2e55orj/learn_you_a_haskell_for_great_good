{-# OPTIONS -Wall -Werror #-}
{- 4.1 -}
maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)
