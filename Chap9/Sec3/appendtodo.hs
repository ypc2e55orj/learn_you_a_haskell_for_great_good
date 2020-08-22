{-# OPTIONS -Wall #-}

{- 9.3 -}
main :: IO ()
main = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")
