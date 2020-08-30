{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

{- p.384 -}

main :: IO ()
main = do
  txt <- T.getContents
  let out = T.unlines . map (T.append "☆") . T.lines $ txt
  T.putStr out
