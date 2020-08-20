{-# OPTIONS -Wall -Werror #-}

import Data.Char
import Data.List

{- 6.2 -}
wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

tails' :: String -> [String]
tails' = scanr (:) []

isIn :: Eq a => [a] -> [a] -> Bool
isIn needle = any (isPrefixOf needle) . tails

shift :: (Int -> Int) -> String -> String
shift f msg = map (chr . f . ord) msg

encode :: Int -> String -> String
encode offset = shift (+ offset)

decode :: Int -> String -> String
decode offset = shift (subtract offset)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

findTo :: Int -> Maybe Int
findTo x = find ((== x) . digitSum) [1 ..]
