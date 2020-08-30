{-# OPTIONS -Wall #-}

module Main where

{- 15.2 -}
data List a = Empty | Cons a (List a) deriving (Show)

type ListZipper a = ([a], [a])


-- >>> let xs = [1, 2, 3, 4]
-- >>> goForward (xs, [])
-- ([2,3,4],[1])
-- >>> goForward ([2, 3, 4], [1])
-- ([3,4],[2,1])
-- >>> goForward ([3, 4], [2, 1])
-- ([4],[3,2,1])
-- >>> goBack ([4], [3, 2, 1])
-- ([3,4],[2,1])
goForward :: ListZipper a -> ListZipper a
goForward (x : xs, lzs) = (xs, x : lzs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, lz : lzs) = (lz : xs, lzs)
