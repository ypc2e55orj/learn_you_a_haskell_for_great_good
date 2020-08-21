{-# OPTIONS -Wall #-}
{- 8.3.1 -}
main :: IO ()
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"
