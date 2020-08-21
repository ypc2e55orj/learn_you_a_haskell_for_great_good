{-# OPTIONS -Wall #-}
{- 7.8 -}

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red"
  show Yellow = "Yellow"
  show Green = "Green"
