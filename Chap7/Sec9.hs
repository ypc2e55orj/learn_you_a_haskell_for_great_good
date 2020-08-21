{-# OPTIONS -Wall #-}
{- 7.9 -}
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

data Tree a = EmptyNode | Node a (Tree a) (Tree a)

instance YesNo (Tree a) where
  yesno EmptyNode = False
  yesno _ = True

data TrafficLight = Red | Yellow | Green

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf v yr nr =
  if yesno v
    then yr
    else nr
