{-# OPTIONS -Wall #-}
{- 13.6.1 -}
listOfTupels :: [(Int, Char)]
listOfTupels = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

class Monad m => MonadPlus' m where
  mzero' :: m a
  mplus' :: m a -> m a -> m a

instance MonadPlus' [] where
  mzero' = []
  mplus' = (++)

guard' :: MonadPlus' m => Bool -> m ()
guard' True = return ()
guard' False = mzero'

sevensOnly :: [Int]
sevensOnly = do
  x <- [1 .. 50]
  guard' ('7' `elem` show x)
  return x
