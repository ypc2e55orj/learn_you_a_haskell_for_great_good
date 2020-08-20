{-# OPTIONS -Wall -Werror #-}
{- 4.3 -}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = quicksort smallOrEqual ++ [x] ++ quicksort large
  where
    smallOrEqual = [x' | x' <- xs, x' <= x]
    large = [x' | x' <- xs, x' > x]
