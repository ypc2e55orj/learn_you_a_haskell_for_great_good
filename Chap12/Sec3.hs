{-# OPTIONS -Wall #-}
{- 12.2 -}
class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'

{- 12.3 -}
instance Monoid' [a] where
  mempty' = []
  mappend' = (++)

newtype Product' a = Product' {getProduct' :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product' a) where
  mempty' = Product' 1
  Product' x `mappend'` Product' y = Product' (x * y)

newtype Sum' a = Sum' {getSum' :: a}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Sum' a) where
  mempty' = Sum' 0
  Sum' x `mappend'` Sum' y = Sum' (x + y)

newtype Any' = Any' {getAny' :: Bool}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' Any' where
  mempty' = Any' False
  Any' x `mappend'` Any' y = Any' (x || y)

newtype All' = All' {getAll' :: Bool}
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' All' where
  mempty' = All' True
  All' x `mappend'` All' y = All' (x && y)

instance Monoid' Ordering where
  mempty' = EQ
  LT `mappend'` _ = LT
  EQ `mappend'` x = x
  GT `mappend'` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  let a = length x `compare` length y
      b = x `compare` y
   in if a == EQ then b else a

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend'` (x `compare` y)

lengthCompareVowels :: String -> String -> Ordering
lengthCompareVowels x y =
  (length x `compare` length y)
    `mappend'` (vowels x `compare` vowels y)
    `mappend'` (x `compare` y)
  where
    vowels = length . filter (`elem` "aeiou")

instance (Monoid' a) => Monoid' (Maybe a) where
  mempty' = Nothing
  Nothing `mappend'` m = m
  m `mappend'` Nothing = m
  Just m1 `mappend'` Just m2 = Just (m1 `mappend'` m2)

newtype First' a = First' {getFirst' :: Maybe a}

instance Monoid' (First' a) where
  mempty' = First' Nothing
  First' (Just x) `mappend'` _ = First' (Just x)
  First' Nothing `mappend'` x = x

newtype Last' a = Last' {getLast' :: Maybe a}

instance Monoid' (Last' a) where
  mempty' = Last' Nothing
  _ `mappend'` Last' (Just x) = Last' (Just x)
  x `mappend'` Last' Nothing = x
