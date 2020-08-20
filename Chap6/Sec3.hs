{-# OPTIONS -Wall -Werror #-}

import Data.Char
import qualified Data.Map as Map

{- 6.3 -}
phoneBook :: [(String, String)]
phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "45202928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

findKey :: Eq k => k -> [(k, v)] -> v
findKey key = snd . head . filter (\(k, _) -> key == k)

findKey' :: Eq k => k -> [(k, v)] -> Maybe v
findKey' _ [] = Nothing
findKey' key ((k, v) : xs)
  | key == k = Just v
  | otherwise = findKey' key xs

findKey'' :: Eq k => k -> [(k, v)] -> Maybe v
findKey'' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

phoneBookMap :: Map.Map String String
phoneBookMap = Map.fromList phoneBook

newPhoneBookMap :: Map.Map String String
newPhoneBookMap = Map.insert "grace" "341-9021" phoneBookMap

stringToDigits :: String -> [Int]
stringToDigits = map digitToInt . filter isDigit

phoneBook' :: [(String, String)]
phoneBook' =
  [ ("betty", "555-2938"),
    ("betty", "342-2493"),
    ("bonnie", "452-2938"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "929-8282"),
    ("penny", "853-2492"),
    ("penny", "555-2111")
  ]

phoneBookToMap :: Ord k => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith ((++) . (++ ", "))

phoneBookToMap' :: Ord k => [(k, a)] -> Map.Map k [a]
phoneBookToMap' = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))
