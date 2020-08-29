{-# OPTIONS -Wall #-}

import Control.Monad.State
import System.Random

{- 14.3.2 -}
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)
