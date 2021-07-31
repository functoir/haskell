module Main where
import Data.Char (toUpper)


-- | File: io.hs
--   Purpose: Basic IO in Haskell
--   Author: siavava <amittaijoel@outlook.com>
--   Date: 07/15/2021

greet :: IO ()
greet = do
  print "What is your name?"
  name <- getLine
  let uname = map toUpper name
  print $ "Hello " ++ uname ++ " ." 


main :: IO ()
main = do
  greet