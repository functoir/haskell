module Main where

import Prelude hiding (Functor(..), fmap, (<$>), (<*>), pure)

import MyFunctor ( Person, tupleFromString, personFromTuple )

main :: IO ()
main = do
  let p = tupleFromString "John Doe 21"
  print p
  let person = personFromTuple p
  print person
  return ()
