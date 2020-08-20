{- 1.6 -}
rightTriangles :: (Num c, Eq c, Enum c) => c -> [(c, c, c)]
rightTriangles x =
  [ (a, b, c)
    | a <- [1 .. 10],
      b <- [1 .. a],
      c <- [1 .. b],
      c ^ 2 + b ^ 2 == a ^ 2,
      a + b + c == x
  ]
