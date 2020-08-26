{-# OPTIONS -Wall #-}
{- 12.12 -}
newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

data CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

newtype CoolBool' = CoolBool' {getCoolBool' :: Bool}

helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"
