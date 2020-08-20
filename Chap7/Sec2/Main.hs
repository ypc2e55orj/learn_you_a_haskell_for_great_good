{-# OPTIONS -Wall -Werror #-}
{- 7.2 -}
import Shapes

main :: IO ()
main = do
  print $ nudge (baseCircle 30) 10 20
