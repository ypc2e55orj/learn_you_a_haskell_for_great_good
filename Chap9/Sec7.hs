{-# OPTIONS -Wall #-}

import Control.Exception
import qualified Data.ByteString.Lazy as B
import System.Directory
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filename : filename' : _) ->
      copy filename filename'
    _ -> return ()

copy :: FilePath -> FilePath -> IO ()
copy src dist = do
  contents <- B.readFile src
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        B.hPutStr tempHandle contents
        hClose tempHandle
        renameFile tempName dist
    )
