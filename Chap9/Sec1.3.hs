{-# OPTIONS -Wall #-}

{- 9.1.3 -}
main :: IO ()
main = do
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter ((< 10) . length) . lines
