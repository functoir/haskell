{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where

import Test.HUnit
import Prelude hiding (getLine, putStr, putStrLn)

-- :set -package HUnit


-- import Test.HUnit

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
  runDuo'
  runDuo act2
  cts <- numTests
  putStrLn ("\n\nPassed Tests: " ++ show cts)

  putStrLn "Testing isLong"
  cts2 <- runTestTT testIsLong
  print cts2
  print $ range 1 10
  let arr = [1..10]
  print $ listIncr arr
  print $ listIncr' arr
  return ()

-- | Get a sentence from `stdin` and return it.
getLine :: IO String
getLine = do
  c <- getChar
  if c == '\n'
    then return []
    else do
      cs <- getLine
      return (c:cs)

-- | Print a string to `stdout`.
putStr :: String -> IO ()
putStr [] = return ()
putStr (c:cs) = do
  putChar c
  putStr cs

-- | Print a string to `stdout` and add a newline.
putStrLn :: String -> IO ()
putStrLn s = do
  putStr s
  putStr "\n"

{-
  Multi-line comment
  
  >>> putStrLn "Hello, Haskell!"

  Anyway, here goes random HUnit tests.
-}

t1 :: Test
t1 = (1 + 2 :: Int) ~?= 3
t2 :: Test
t2 = (1 + 2 :: Int) ~?= 4

numTests :: IO Counts
numTests = runTestTT $ TestList [t1, t2]

{-! 
  Structured Data

  1. Tuples:
    - We can put anything in a tuple! Even functions.
-}

act2 :: (IO (), IO ())
act2 = (putStr "Hello", putStr "World")

-- | A function that takes two functions (in a tuple) as arguments and runs them.
-- | Experiment with different orders!
runDuo :: (IO (), IO()) -> IO ()
runDuo (fnA, fnB) = do
  fnA   -- ?function A
  fnB   -- ?function B

runDuo' :: IO ()
runDuo' = do
  let (fnA, fnB) = act2
  fnA
  fnB

{-! 
  Structured Data

  2. Lists:
    - Lists are homogeneous.
    - Lists are ordered.
    - Lists are indexed.
    - Lists are infinite.
    - Lists are lazy.
-}

-- | Function that takes range of numbers and returns a list of them.
range :: Int -> Int -> [Int]
range a b
  | a > b = []
  | otherwise = a : range (a + 1) b

isLong :: [a] -> Bool
isLong (_:_:_) = True
isLong _ = False

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False


testIsLong :: Test
testIsLong = TestList [ not (isLong [])   ~? "nil",    -- can convert booleans to tests by naming them via `~?`
                       not (isLong "a")  ~? "one",
                       not (isLong "ab") ~? "two",
                       isLong "abc"      ~? "three" ]

listIncr :: Num a => [a] -> [a]
listIncr [] = []
listIncr (x:xs) = (x + 1) : listIncr xs

listIncr' :: Num a => [a] -> [a]
listIncr' = map (+1)
