{-# OPTIONS -Wall #-}

import System.Random

{- 9.6.1 -}
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, firstGen) = random gen
      (secondCoin, secondGen) = random firstGen
      (thirdCoin, _) = random secondGen
   in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (valueList, finalGen) = finiteRandoms (n - 1) newGen
   in (value : valueList, finalGen)

-- >>> randomR ((1, 6) :: (Int, Int)) (mkStdGen 100)
-- (6,4041414 40692)

-- >>> take 10 $ randomRs ('a', 'z') (mkStdGen 3) :: [Char]
-- "xnuhlfwywq"
