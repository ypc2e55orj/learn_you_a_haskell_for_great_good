{-# OPTIONS -Wall #-}
{- 7.7 -}
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

data List' a = Empty' | Cons' {listHead :: a, listTail :: List a}
  deriving (Show, Read, Eq, Ord)

infixr 5 :-:

data List'' a = Empty'' | a :-: (List'' a) deriving (Show, Read, Eq, Ord)

infixr 5 +-+

(+-+) :: List'' a -> List'' a -> List'' a
Empty'' +-+ ys = ys
(x :-: xs) +-+ ys = x :-: (xs +-+ ys)

data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyNode EmptyNode

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyNode = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: Ord a => a -> Tree a -> Bool
treeElem _ EmptyNode = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

numsTree :: Tree Int
numsTree = foldr treeInsert EmptyNode [8, 6, 4, 1, 7, 3, 5]
