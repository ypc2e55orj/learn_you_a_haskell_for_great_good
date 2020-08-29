{-# OPTIONS -Wall #-}

{- 14.4 (13.4') -}
type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
  | abs (left' - right) < 4 = Right (left + n, right)
  | otherwise = Left ("left: " ++ show left' ++ ", right: " ++ show right)
  where
    left' = left + n

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
  | abs (right' - left) < 4 = Right (left, right + n)
  | otherwise = Left ("left: " ++ show left ++ ", right: " ++ show right')
  where
    right' = right + n

banana :: Pole -> Either String Pole
banana (l, r) = Left ("banana left: " ++ show l ++ ", right: " ++ show r)

routine :: Either String Pole
routine = return (0, 0) >>= landLeft 2 >>= landRight 2 >>= landLeft 1

{- 13.5 -}
routine' :: Either String Pole
routine' = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

routine'' :: Either String Pole
routine'' = do
  start <- return (0, 0)
  first <- landLeft 2 start
  d <- banana first
  second <- landRight 2 d
  landLeft 1 second
