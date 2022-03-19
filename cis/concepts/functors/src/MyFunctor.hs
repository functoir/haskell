{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

-- | MyFunctor: Implementation of the Functor class.
module MyFunctor (
    Person
  , tupleFromString
  , personFromTuple
  
) where

import Prelude hiding (Functor(..), fmap, (<$>), (<*>), pure)

-- | Generate tuple from a string.
tupleFromString :: String -> Maybe (String, String, Int)
tupleFromString str
  | length components /= 3 = Nothing
  | otherwise = Just (head components, second components, age)
    where
      second = head . tail
      components = words str
      age = read (components !! 2) :: Int

-- | Person with first name, last name, and age.
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int
}


personFromTuple :: (String, String, Int) -> Person
personFromTuple (fname, lname, age) = Person fname lname age
