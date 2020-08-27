{-# OPTIONS -Wall #-}
{- 13.7 -}
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \x -> g x >>= f
