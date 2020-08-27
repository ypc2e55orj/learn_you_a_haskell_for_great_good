{-# OPTIONS -Wall #-}

import Control.Monad.Writer

{- 14.1 -}
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, l) f = let (y, l') = f x in (y, l ++ l')

applyLog' :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog' (x, l) f = let (y, l') = f x in (y, l `mappend` l')

type Food = String

type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("wiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b)

gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    let res = (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show res]
    gcd'' a res

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    let m = a `mod` b
    result <- gcdReverse b m
    tell [show a ++ " mod " ++ show b ++ " = " ++ show m]
    return result

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

instance Show a => Show (DiffList a) where
  show x = "DiffList " ++ show (fromDiffList x)

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  DiffList f <> DiffList g = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  mappend = (<>)

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    let m = a `mod` b
    result <- gcdReverse' b m
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show m])
    return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown n = do
  finalCountDown (n - 1)
  tell (toDiffList [show n])

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' n = do
  finalCountDown' (n - 1)
  tell [show n]
