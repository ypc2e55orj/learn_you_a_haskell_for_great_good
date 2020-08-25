main :: IO ()
main = do
  a <- pure (++) <*> getLine <*> getLine
  putStrLn a
