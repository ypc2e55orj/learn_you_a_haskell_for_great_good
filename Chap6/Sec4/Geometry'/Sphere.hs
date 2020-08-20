{-# OPTIONS -Wall -Werror #-}
{- 6.4 -}

module Geometry'.Sphere (volume, area) where

volume :: Float -> Float
volume = (* (4 / 3)) . (* pi) . (** 2)

area :: Float -> Float
area = (** 2)
