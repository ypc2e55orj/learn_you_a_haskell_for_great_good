{-# OPTIONS -Wall -Werror #-}

{- 6.4 -}
module Geometry
  ( sphereVolume,
    sphereArea,
    cubeVolume,
    cubeArea,
    cuboidArea,
    cuboidVolume,
  )
where

sphereVolume :: Float -> Float
sphereVolume = (* (4 / 3)) . (* pi) . (** 3)

sphereArea :: Float -> Float
sphereArea = (* 4) . (* pi) . (** 2)

rectArea :: Float -> Float -> Float
rectArea = (*)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectArea a b * 2 + rectArea b c * 2 + rectArea c a * 2
