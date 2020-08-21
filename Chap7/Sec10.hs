{-# OPTIONS -Wall #-}
{- 7.10 -}

data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyNode EmptyNode

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyNode = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

instance Functor Tree where
  fmap _ EmptyNode = EmptyNode
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
