{-# OPTIONS -Wall #-}

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap _ CNothing = CNothing
  fmap f (CJust c a) = CJust (c + 1) (f a)
