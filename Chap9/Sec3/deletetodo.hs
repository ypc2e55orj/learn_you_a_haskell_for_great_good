{-# OPTIONS -Wall #-}

import Data.List
import System.Directory
import System.IO

{- 9.3 -}
main :: IO ()
main = do
  contents <- readFile "todo.txt"

  let todoTasks = lines contents
      numberedTasks =
        zipWith
          (\n line -> show (n :: Int) ++ " - " ++ line)
          [0 ..]
          todoTasks
  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTasks

  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString :: Int
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks

  (tempName, tempHandle) <- openTempFile "." "temp"

  hPutStr tempHandle newTodoItems
  hClose tempHandle
  removeFile "todo.txt"

  renameFile tempName "todo.txt"
