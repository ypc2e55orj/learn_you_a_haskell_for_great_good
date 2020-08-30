{-# OPTIONS -Wall #-}

module Main where

{- 15.4 -}
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

(-:) :: a -> (a -> b) -> b
x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type TreeZipper a = (Tree a, [Crumb a])

-- >>> import Control.Monad
-- >>> return (freeTree, []) >>= foldr (>=>) return (replicate 10 goLeft)
-- Nothing
-- >>> import Control.Monad
-- >>> return (freeTree, []) >>= foldr (>=>) return (replicate 3 goLeft)
-- Just (Node 'N' Empty Empty,[LeftCrumb 'L' (Node 'T' Empty Empty),LeftCrumb 'O' (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)),LeftCrumb 'P' (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)))])
goLeft :: TreeZipper a -> Maybe (TreeZipper a)
goLeft (Node x l r, tcs) = Just (l, (LeftCrumb x r) : tcs)
goLeft (Empty, _) = Nothing

goRight :: TreeZipper a -> Maybe (TreeZipper a)
goRight (Node x l r, tcs) = Just (r, (RightCrumb x l) : tcs)
goRight (Empty, _) = Nothing

-- >>> return (freeTree, []) >>= goRight
-- Just (Node 'L' (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty)) (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty)),[RightCrumb 'P' (Node 'O' (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty)) (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty)))])
-- >>> return (freeTree, []) >>= goUp
-- Nothing
goUp :: TreeZipper a -> Maybe (TreeZipper a)
goUp (t, (LeftCrumb x r) : tcs) = Just (Node x t r, tcs)
goUp (t, (RightCrumb x l) : tcs) = Just (Node x l t, tcs)
goUp (_, []) = Nothing
