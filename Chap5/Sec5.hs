{-# OPTIONS -Wall -Werror #-}
{- 5.5 -}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ acc [] = acc
myFoldl f acc (x : xs) = myFoldl f (f acc x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x : xs) = f x (myFoldr f acc xs)

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f = myFoldr (\x acc -> f x : acc) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = myFoldr (\x' acc -> if x == x' then True else acc) False xs

reverse' :: [a] -> [a]
reverse' = myFoldl (flip (:)) []

reverse'' :: [a] -> [a]
reverse'' = myFoldr (\x -> (++ [x])) []

product' :: Num a => [a] -> a
product' = myFoldl (*) 1

product'' :: Num a => [a] -> a
product'' = myFoldr (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = myFoldr (\x acc -> if p x then x : acc else acc) []

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 _ [] = error "empty list"
myFoldl1 f (acc : xs) = myFoldl f acc xs

last' :: [a] -> a
last' = myFoldl1 (\_ x -> x)

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 _ [] = error "empty list"
myFoldr1 _ [acc] = acc
myFoldr1 f (x : xs) = f x (myFoldr1 f xs)

and' :: [Bool] -> Bool
and' = myFoldr1 (&&)

myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl _ acc [] = [acc]
myScanl f acc (x : xs) = acc : myScanl f (f acc x) xs

myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr _ acc [] = [acc]
myScanr f acc (x : xs) = f x y : ys
  where
    ys@(y : _) = myScanr f acc xs

myScanl1 :: (a -> a -> a) -> [a] -> [a]
myScanl1 _ [] = error "empty list"
myScanl1 _ [acc] = [acc]
myScanl1 f (x : xs) = x : myScanl1 f xs

myScanr1 :: (a -> a -> a) -> [a] -> [a]
myScanr1 _ [] = error "empty list"
myScanr1 _ [acc] = [acc]
myScanr1 f (x : xs) = f x y : ys
  where
    ys@(y : _) = myScanr1 f xs

sqrtSums :: Int
sqrtSums = length (takeWhile (< (1000.0 :: Double)) (myScanl1 (+) (map sqrt [1 ..])))
