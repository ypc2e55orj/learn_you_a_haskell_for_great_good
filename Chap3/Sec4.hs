{-# OPTIONS -Wall -Werror #-}
{- 3.4 -}
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ** 2
   in sideArea + 2 * topArea

clacBmis :: [(Double, Double)] -> [Double]
clacBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ** 2]

clacFatBmis :: [(Double, Double)] -> [Double]
clacFatBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ** 2, bmi > 25.0]
