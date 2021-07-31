module Main where

-- | File: records.hs
--   Purpose: basic records in Haskell
--   Author: siavava <amittaijoel@outlook.com>
--   Date: 07/15/2021


data Person = Person {
  name :: String,
  age :: Integer
} deriving (Show, Eq)

greet :: Person -> [Char]
greet (Person name _) = "Hi " ++ name

main :: IO ()
main = do
  let person = Person "Amittai" 20
  let person2 = Person "Amittai" 20
  let person3 = Person "Jay" 20
  let person4 = Person "Amittai" 30
  print $ greet person
  print person

  print "Equivalence"
  print $ "1 == 2: " ++ show (person == person2)
  print $ "1 == 3: " ++ show (person == person3)
  print $ "1 == 4: " ++ show (person == person4)

