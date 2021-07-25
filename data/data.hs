module Main where

-- import Prelude hiding (Either, Left, Right)
import Data.Either -- ( isLeft, fromLeft, isRight, partitionEithers)

-- Data :: can have named fields, which can be accessed as named.
-- Data can also have nultiple constructors.
data Color = RGB {
  r :: Int,
  g :: Int,
  b :: Int
} deriving (Eq, Show)

type ColorT = (Color, Color, Color)   -- just an alias, usage of ColorT is same as the tuple

-- newtype :: can only have a single constructor. Else similar to data. 
newtype Name = Name String

-- data Either a b = Left a | Right b              -- import Data.Either

-- lefts :: [Either a b] -> [a]                    -- extract values from Either
-- lefts xs = [x | x <- xs, isLeft x]

-- rights :: [Either a b] -> [b]                   -- extract values from Either
-- rights xs = [fromLeft x | x <- xs, isRight x]





main :: IO ()
main = do
  print "Hello"

  let eithers = [Left 1, Right '2', Left 3, Right '4', Left 5, Right '6']
  print $ partitionEithers eithers