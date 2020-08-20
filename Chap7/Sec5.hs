{-# OPTIONS -Wall -Werror #-}
{- 7.5 -}
data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Eq, Show, Read)

mikeD :: Person
mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}

adRock :: Person
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}

mca :: Person
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}

mysteryDudeString :: String
mysteryDudeString =
  "Person' {firstName' = \"Michael\",\
  \ lastName' = \"Diamond\",\
  \ age' = 43}"

mysteryDudePerson' :: Person
mysteryDudePerson' = read mysteryDudeString :: Person

{-
-- ghci
*Main> mysteryDudePerson' == (read mysteryDudeString :: Person')
True
-}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
