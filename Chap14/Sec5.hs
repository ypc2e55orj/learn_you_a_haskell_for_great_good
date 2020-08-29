{-# OPTIONS -Wall #-}

module Main where

import Control.Monad.Writer

{- 14.5 -}

-- >>> liftM (*3) (Just 8)
-- Just 24

-- >>> fmap (*3) (Just 8)
-- Just 24

-- >>> runWriter $ fmap not $ writer (True, "chickpeas")
-- (False,"chickpeas")

-- >>> runWriter $ liftM not $ writer (True, "chickpeas")
-- (False,"chickpeas")

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = do
  x <- m
  return (f x)

liftM'' :: Monad m => (a -> b) -> m a -> m b
liftM'' f m = m >>= (\x -> return (f x))

-- >>> (+) <$> Just 3 <*> Just 5
-- Just 8

-- >>> (+) <$> Just 3 <*> Nothing
-- Nothing

-- >>> Just (+3) <*> Just 4
-- Just 7


-- >>> Just (+3) `ap` Just 4
-- Just 7

-- >>> [(+1), (+2), (+3)] <*> [10, 11]
-- [11,12,12,13,13,14]


-- >>> [(+1), (+2), (+3)] `ap` [10, 11]
-- [11,12,12,13,13,14]


-- >>> join (Just (Just 9))
-- Just 9


-- >>> join (Just (Nothing)) :: Maybe Int
-- Nothing

-- >>> join Nothing :: Maybe Int
-- Nothing

-- >>> join [[1, 2, 3], [4, 5, 6]]
-- [1,2,3,4,5,6]

-- >>> runWriter $ join (writer (writer (1, "aaa"), "bbb"))
-- (1,"bbbaaa")


-- >>> join (Right (Right 9)) :: Either String Int
-- Right 9

-- >>> join (Right (Left "error")) :: Either String Int
-- Left "error"

-- >>> join (Left "error") :: Either String Int
-- Left "error"

-- >>> fst $ runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3]
-- [1,2,3]
-- >>> snd $ runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3]
-- ["9 is too large, throwing it away","Keeping 1","5 is too large, throwing it away","Keeping 2","10 is too large, throwing it away","Keeping 3"]
keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

-- >>> powerset [1, 2, 3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [True, False])

-- >>> foldM binSmalls 0 [2, 8, 3, 1]
-- Just 14
-- >>> foldM binSmalls 0 [2, 11, 3, 1]
-- Nothing
binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)
