{-# OPTIONS -Wall #-}

import Control.Applicative

{- 11.4 -}
liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$> a <*> b

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x : xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: Applicative f => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])
