{-# OPTIONS -Wall #-}

{- 11.3.1 -}
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b
  (<$/) :: a -> f b -> f a
  (<$/) = fmap' . const

class Functor' f => Applicative' f where
  pure' :: a -> f a
  (<*/>) :: f (a -> b) -> f a -> f b

instance Functor' Maybe where
  fmap' f (Just x) = Just (f x)
  fmap' _ Nothing = Nothing

instance Applicative' Maybe where
  pure' = Just
  Nothing <*/> _ = Nothing
  (Just f) <*/> x = fmap' f x

instance Functor' [] where
  fmap' = map

instance Applicative' [] where
  pure' x = [x]
  (<*/>) fs xs = [f x | f <- fs, x <- xs]

instance Functor' IO where
  fmap' f x = do
    x' <- x
    return (f x')

instance Applicative' IO where
  pure' = return
  (<*/>) a b = do
    f <- a
    x <- b
    return (f x)

instance Functor' ((->) r) where
  fmap' = (.)

instance Applicative' ((->) r) where
  pure' x = \_ -> x
  f <*/> g = \x -> f x (g x)
