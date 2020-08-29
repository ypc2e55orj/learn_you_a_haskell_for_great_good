{-# OPTIONS -Wall #-}
{- 14.2 -}

addStuff :: Int -> Int
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)
