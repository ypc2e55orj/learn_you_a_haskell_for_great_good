{-# OPTIONS -Wall #-}

import Control.Monad

{- 13.6.1 -}
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r - 1),
      (c + 2, r + 1),
      (c - 2, r - 1),
      (c - 2, r + 1),
      (c + 1, r - 2),
      (c + 1, r + 2),
      (c - 1, r - 2),
      (c - 1, r + 2)
      ]

  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])

  return (c', r')

moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c, r) =
  filter
    onBoard
    [ (c + 2, r - 1),
      (c + 2, r + 1),
      (c - 2, r - 1),
      (c - 2, r + 1),
      (c + 1, r - 2),
      (c + 1, r + 2),
      (c - 1, r - 2),
      (c - 1, r + 2)
    ]
  where
    onBoard (c', r') = c' `elem` [1 .. 8] && r' `elem` [1 .. 8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight' start
  second <- moveKnight' first
  moveKnight' second

in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight' >>= moveKnight' >>= moveKnight'

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3' start

routeReachIn3 :: KnightPos -> KnightPos -> [[KnightPos]]
routeReachIn3 start end = do
  first <- moveKnight' start
  second <- moveKnight' first
  third <- moveKnight' second

  guard (end == third)

  return [first, second, third]
