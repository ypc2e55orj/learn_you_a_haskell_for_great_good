{-# OPTIONS -Wall #-}

import Control.Monad (when)
import System.Random

{- 9.6.4 -}
main :: IO ()
main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)

  putStrLn "Which number in the range from 1 to 10 am I thinking of?"

  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString :: Int
    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    askForNumber newGen
