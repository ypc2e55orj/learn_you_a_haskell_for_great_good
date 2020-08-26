{-# OPTIONS -Wall #-}

{- 13.5 -}
foo :: Maybe String
foo =
  Just (3 :: Int)
    >>= ( \x ->
            Just "!"
              >>= ( \y ->
                      Just (show x ++ y)
                  )
        )

foo' :: Maybe String
foo' = do
  x <- Just (3 :: Int)
  y <- Just "!"
  Just (show x ++ y)

justH :: Maybe Char
justH = do
  (x : _) <- Just "hello"
  return x

wopwop :: Maybe Char
wopwop = do
  (x : _) <- Just ""
  return x
