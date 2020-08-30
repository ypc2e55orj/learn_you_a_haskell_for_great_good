{-# OPTIONS -Wall #-}

module Main where

{- 15.1 -}
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

data Direction = NodeLeft | NodeRight deriving (Show)

type Directions = [Direction]

-- >>> let newTree = changeTo 'P' [NodeRight, NodeLeft] freeTree
-- >>> elemAt [NodeRight, NodeLeft] newTree
-- 'P'
changeTo :: a -> Directions -> Tree a -> Tree a
changeTo rep [] (Node _ l r) = Node rep l r
changeTo rep (NodeLeft : ds) (Node x l r) = Node x (changeTo rep ds l) r
changeTo rep (NodeRight : ds) (Node x l r) = Node x l (changeTo rep ds r)

elemAt :: Directions -> Tree a -> a
elemAt (NodeLeft : ds) (Node _ l _) = elemAt ds l
elemAt (NodeRight : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

type Breadcrumbs = [Direction]

-- >>> goLeft (goRight (freeTree, []))
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[NodeLeft,NodeRight])
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, NodeLeft : bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, NodeRight : bs)

-- >>> (freeTree, []) -: goRight -: goLeft
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[NodeLeft,NodeRight])
(-:) :: a -> (a -> b) -> b
x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs' a = [Crumb a]

goLeft' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goLeft' (Node x l r, bs) = (l, LeftCrumb x r : bs)

goRight' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goRight' (Node x l r, bs) = (r, RightCrumb x l : bs)

goUp :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goUp (t, LeftCrumb x r : bs) = (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = (Node x t l, bs)

type Zipper a = (Tree a, Breadcrumbs' a)

-- >>> let newFocus = (freeTree, []) -: goLeft' -: goRight' -: modify (\_ -> 'P')
-- >>> let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')
-- >>> newFocus2
-- (Node 'X' (Node 'P' (Node 'S' Empty Empty) (Node 'A' Empty Empty)) (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)),[LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

-- >>> let farLeft = (freeTree, []) -: goLeft' -: goLeft' -: goLeft' -: goLeft'
-- >>> let newFocus = farLeft -: attach (Node 'Z' Empty Empty)
-- >>> newFocus
-- (Node 'Z' Empty Empty,[LeftCrumb 'N' Empty,LeftCrumb 'L' (Node 'T' Empty Empty),LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)
