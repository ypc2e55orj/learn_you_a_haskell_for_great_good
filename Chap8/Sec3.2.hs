{-# OPTIONS -Wall #-}

import Data.Char

{- 8.3.2 -}
main :: IO ()
main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last naee?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastNeme = map toUpper lastName
  putStrLn $
    "hey " ++ bigFirstName ++ " "
      ++ bigLastNeme
      ++ ", how are you?"
