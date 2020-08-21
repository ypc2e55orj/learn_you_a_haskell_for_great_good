{-# OPTIONS -Wall #-}

import Control.Monad
import Data.Char

{- 8.4 -}
main :: IO ()
main = do
  -- putStr
  putStrLn "### putStr"
  putStr "Hey "
  putStr "I'm "
  putStrLn "Andy!"

  -- putChar
  putStrLn "### putChar"
  putChar 't'
  putChar 'e'
  putChar 'h'

  -- putStr'
  putStr' "### putStr test!"

  -- print
  putStrLn "### print"
  print True
  print (2 :: Int)
  print "haha"
  print (3.2 :: Double)
  print ([3, 4, 3] :: [Int])

  -- when (Control.Monad)
  putStrLn "### when"
  input <- getLine
  when (input == "SWORDFISH") $ do
    putStrLn input

  -- sequence
  putStrLn "### sequence"
  rs <- sequence [getLine, getLine, getLine]
  print rs

  -- mapM
  putStrLn "### mapM, mapM_"
  mapM print ([1, 2, 3] :: [Int]) -- [(), (), ()]
  mapM_ print ([1, 2, 3] :: [Int]) -- ()

  -- forM (Control.Monad)
  putStrLn "### forM"
  colors <- forM ([1, 2, 3, 4] :: [Int]) $ \n -> do
    putStrLn $
      "Which color do you associate with the number "
        ++ show n
        ++ "?"
    getLine

  putStrLn "The colors that you associate with 1, 2, 3 and 4 are:"
  mapM_ putStrLn colors

  -- forever
  forever $ do
    l <- getLine
    putStrLn $ map toUpper l

  return ()

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs
