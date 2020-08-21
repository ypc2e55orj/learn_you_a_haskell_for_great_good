{-# OPTIONS -Wall #-}
{- 8.3.4 -}
main :: IO ()
main = do
  return ()
  return "HAHAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return (4 :: Int)
  putStrLn line
