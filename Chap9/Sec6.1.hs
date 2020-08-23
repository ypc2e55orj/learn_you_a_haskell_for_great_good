{-# OPTIONS -Wall #-}

import System.Random

{- 9.6.1 -}
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, firstGen) = random gen
      (secondCoin, secondGen) = random firstGen
      (thirdCoin, _) = random secondGen
   in (firstCoin, secondCoin, thirdCoin)
