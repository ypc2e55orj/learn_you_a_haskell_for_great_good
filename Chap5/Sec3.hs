{-# OPTIONS -Wall -Werror #-}
{- 5.3 -}
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerOrEqual = filter' (<= x) xs
      larger = filter' (> x) xs
   in quicksort smallerOrEqual ++ [x] ++ quicksort larger

largestDivisible :: Integer
largestDivisible = head (filter (p) [1000000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain x
  | even x = x : chain (x `div` 2)
  | odd x = x : chain (x * 3 + 1)
  | otherwise = []

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 15

{- 5.4 -}
numLongChains' :: Int
numLongChains' = length (filter ((> 15) . length) (map chain [1 .. 100]))
