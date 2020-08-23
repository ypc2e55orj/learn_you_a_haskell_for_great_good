{-# OPTIONS -Wall #-}

import System.Random

{- 9.6.2 -}
main :: IO ()
main = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
