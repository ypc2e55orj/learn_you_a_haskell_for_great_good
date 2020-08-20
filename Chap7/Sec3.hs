{-# OPTIONS -Wall -Werror #-}
{- 7.3 -}
{- firstname lastname age height phone flavor -}
{-
data Person = Person String String Int Float String String
  deriving (Show)

firstName :: Person -> String
firstName (Person n _ _ _ _ _) = n

lastName :: Person -> String
lastName (Person _ n _ _ _ _) = n

age :: Person -> Int
age (Person _ _ a _ _ _) = a

height :: Person -> Float
height (Person _ _ _ h _ _) = h

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ n _) = n

flavor :: Person -> String
flavor (Person _ _ _ _ _ f) = f
-}

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

-- data Car = Car String String Int deriving (Show)

data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

car :: Car
car = Car {company = "Ford", model = "Mustang", year = 1967}

otherCar :: Car
otherCar = car {year = 2013}

{- 7.4 -}
data Maybe' a = Just' a | Nothing'

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- bad {
data Car' a b c = Car'
  { company' :: a,
    model' :: b,
    year' :: c
  }
  deriving (Show)

tellCar' :: Show a => Car' String String a -> String
tellCar' (Car' {company' = c, model' = m, year' = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- }

data Vector a = Vector a a a deriving (Show)

vplus :: Num a => Vector a -> Vector a -> Vector a
vplus (Vector a1 a2 a3) (Vector b1 b2 b3) = Vector (a1 + b1) (a2 + b2) (a3 + b3)

dotProd :: Num a => Vector a -> Vector a -> a
dotProd (Vector a1 a2 a3) (Vector b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3

vmult :: Num a => Vector a -> a -> Vector a
vmult (Vector a1 a2 a3) m = Vector (a1 * m) (a2 * m) (a3 * m)
