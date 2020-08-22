{-# OPTIONS -Wall #-}

import Control.Exception
import Data.List
import System.Directory
import System.Environment
import System.IO

{- 9.5 -}
addNumberToTasks :: [String] -> [String]
addNumberToTasks = zipWith ((++) . (++ " ") . show) [(0 :: Int) ..]

writeTodoItems :: FilePath -> String -> IO ()
writeTodoItems fileName todoItems = do
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        hPutStr tempHandle todoItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName
    )

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName $ todoItem ++ "\n"
add _ = return ()

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName

  let todoTasks = lines contents
      numberedTasks = addNumberToTasks todoTasks
  putStrLn "These are your TO-DO items:"
  putStr $ unlines numberedTasks
view _ = putStrLn "Too few arguments or too many arguments."

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName

  let todoTasks = lines contents
      number = read numberString :: Int
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks

  writeTodoItems fileName newTodoItems
remove _ = putStrLn "Too few arguments or too many arguments."

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- readFile fileName

  let todoTasks = lines contents
      number = read numberString :: Int
      specificItem = todoTasks !! number
      newTodoItems = unlines $ specificItem : delete specificItem todoTasks

  writeTodoItems fileName newTodoItems
bump _ = putStrLn "Too few arguments or too many arguments."

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command doesn't exist"

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = doesntExist command

main :: IO ()
main = do
  contents <- getArgs
  case contents of
    (x : xs) -> dispatch x xs
    _ -> putStrLn "Too few arguments."
