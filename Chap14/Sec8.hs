{-# OPTIONS -Wall #-}

module Main where

import Data.Ratio

{- 14.8 -}
-- >>> 1%4
-- 1 % 4
-- >>> 1%2 + 1%2
-- 1 % 1
-- >>> 1%3 + 5%4
-- 19 % 12

newtype Probe a = Probe {getProbe :: [(a, Rational)]} deriving (Show)

instance Functor Probe where
  fmap f (Probe xs) = Probe $ map (\(x, p) -> (f x, p)) xs

-- >>> fmap negate (Probe [(3, 1%2), (5, 1%4), (9, 1%4)])
-- Probe {getProbe = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}

thisSituation :: Probe (Probe Char)
thisSituation =
  Probe
    [ (Probe [('a', 1 % 2), ('b', 1 % 2)], 1 % 4),
      (Probe [('c', 1 % 2), ('d', 1 % 2)], 3 % 4)
    ]

-- >>> flatten thisSituation
-- Probe {getProbe = [('a',1 % 8),('b',1 % 8),('c',3 % 8),('d',3 % 8)]}
flatten :: Probe (Probe a) -> Probe a
flatten (Probe xs) = Probe $ concat $ map multAll xs
  where
    multAll (Probe innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

-- >>> pure id <*> Probe [(1, 1%2), (2, 1%2)]
-- Probe {getProbe = [(1,1 % 2),(2,1 % 2)]}
-- >>> pure (.) <*> Probe [((+1), 5%6), ((+2), 1%6)] <*> Probe [((+3), 1%9), ((+4), 2%9), ((+5), 2%3)] <*> Probe [(5, 1%2), (6, 1%2)]
-- Probe {getProbe = [(9,5 % 108),(10,5 % 108),(10,5 % 54),(11,5 % 54),(11,5 % 18),(12,5 % 18),(10,1 % 108),(11,1 % 108),(11,1 % 54),(12,1 % 54),(12,1 % 18),(13,1 % 18)]}
-- >>> Probe [((+1), 5%6), ((+2), 1%6)] <*> (Probe [((+3), 1%9), ((+4), 2%9), ((+5), 2%3)] <*> Probe [(5, 1%2), (6, 1%2)])
-- Probe {getProbe = [(9,5 % 108),(10,5 % 108),(10,5 % 54),(11,5 % 54),(11,5 % 18),(12,5 % 18),(10,1 % 108),(11,1 % 108),(11,1 % 54),(12,1 % 54),(12,1 % 18),(13,1 % 18)]}
-- >>> pure (+2) <*> pure (3) :: Probe Int
-- Probe {getProbe = [(5,1 % 1)]}
-- >>> pure ((+2) 3) :: Probe Int
-- Probe {getProbe = [(5,1 % 1)]}
-- >>> Probe [((+1), 5%6), ((+2), 1%6)] <*> pure (2)
-- Probe {getProbe = [(3,5 % 6),(4,1 % 6)]}
-- >>> pure ($ 2) <*> Probe [((+1), 5%6), ((+2), 1%6)]
-- Probe {getProbe = [(3,5 % 6),(4,1 % 6)]}
instance Applicative Probe where
  pure x = Probe [(x, 1 % 1)]
  Probe fs <*> Probe xs = Probe [(f x, fr * xr) | (f, fr) <- fs, (x, xr) <- xs]

instance Monad Probe where
  return x = Probe [(x, 1 % 1)]
  m >>= f = flatten (fmap f m)

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Probe Coin
coin = Probe [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Probe Coin
loadedCoin = Probe [(Heads, 1 % 10), (Tails, 9 % 10)]

-- >>> getProbe flipThree
-- [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]
flipThree :: Probe Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a, b, c])
