{-# OPTIONS -Wall -Werror #-}
{- 3.5 -}
head' :: [a] -> a
head' []      = error "No head for empty lists!"
head' (x : _) = x

head'' :: [a] -> a
head'' xs = case xs of
  []      -> error "No head for empty lists!"
  (x : _) -> x

describeList :: [a] -> String
describeList ls =
  "The list is "
    ++ case ls of
      []  -> "empty."
      [_] -> "a singleton list."
      _   -> "a longer list."

describeList' :: [a] -> String
describeList' ls =
  "The list is "
    ++ what ls
  where
    what []  = "empty"
    what [_] = "a singleton list."
    what _   = "a longer list."
