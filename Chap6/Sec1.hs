{-# OPTIONS -Wall -Werror #-}
{- 6.1 -}
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x : xs) = x : nub' (filter (x /=) xs)

nub'' :: Eq a => [a] -> [a]
nub'' [] = []
nub'' (x : xs) = x : nub'' (f xs)
  where
    f [] = []
    f (y : ys)
      | x == y = f ys
      | otherwise = y : f ys

numUniques :: Eq a => [a] -> Int
numUniques = length . nub'
