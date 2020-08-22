{-# OPTIONS -Wall #-}

import Control.Exception
import System.IO

{- 9.2.2 -}
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' p m = bracket (openFile p m) hClose

main :: IO ()
main = do
  withFile' "baabaa.txt" ReadMode $ \h -> do
    c <- hGetContents h
    putStr c
