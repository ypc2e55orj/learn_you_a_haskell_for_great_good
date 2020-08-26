{-# OPTIONS -Wall #-}

{- 13.3 -}
class Monad' m where
  return' :: a -> m a
  (>>=\) :: m a -> (a -> m b) -> m b
  (>>\) :: m a -> m b -> m b
  x >>\ y = x >>=\ \_ -> y
  fail' :: String -> m a
  fail' msg = error msg

instance Monad' Maybe where
  return' = Just
  Nothing >>=\ _ = Nothing
  Just x >>=\ f = f x
  fail' _ = Nothing
