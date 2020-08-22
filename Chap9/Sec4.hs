{-# OPTIONS -Wall #-}

import System.Environment

{- 9.4 -}
main :: IO ()
main = do
  args <- getArgs
  prog <- getProgName
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The program name is:"
  putStrLn prog
