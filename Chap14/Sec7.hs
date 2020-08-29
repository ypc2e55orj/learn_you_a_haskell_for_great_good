{-# OPTIONS -Wall #-}

module Main where

import Control.Monad

{- 14.7(13.6.2') -}
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) =
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

-- >>> moveKnightMany 3 (6, 1)
moveKnightMany :: Int -> KnightPos -> [KnightPos]
moveKnightMany x start = return start >>= foldr (>=>) return (replicate x moveKnight)

-- >>> canReachIn 3 (6, 1) (6, 2)
-- True
canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` moveKnightMany x start
