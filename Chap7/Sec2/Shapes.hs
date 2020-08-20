{-# OPTIONS -Wall -Werror #-}
{- 7.2 -}

module Shapes
  ( Point {-(..)-},
    Shape {-(..)-},
    area,
    nudge,
    baseCircle,
    baseRect,
  )
where

data Point = Point Float Float deriving (Show)

data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ** 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) x' y' = Circle (Point (x + x') (y + y')) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x' y' =
  Rectangle (Point (x1 + x') (y1 + y')) (Point (x2 + x') (y2 + y'))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
