{-# OPTIONS -Wall #-}

{- 8.3.3 -}
main :: IO ()
main = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
