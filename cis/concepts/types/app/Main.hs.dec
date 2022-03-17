{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
module Main where

import Prelude hiding (lookup)

import Data.Char      -- Character operations
-- import Data.Maybe hiding (fromMaybe)     -- Maybe operations

main :: IO ()
main = do
  let day1 = Monday
  print $ show day1
  print $ show $ nextDay day1
  print $ show (Monday > Tuesday)

  let zero = Zero
  let num1 = Succ zero
  let num2 = Succ num1

  print $ show zero
  print $ show num1
  print $ show num2

  let num4 = num1 `natPlus` num2
  print $ show num4 ++ "= " ++ show (natToInt num4)

  -- cyphers
  putStrLn "What file should I encode and decode?"
  filename <- getLine
  processFile filename


data Day =
  Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

nextDay :: Day -> Day
nextDay day
  | day == maxBound = minBound
  | otherwise = succ day

data Shape =
  Circle Float Float Float
  | Rectangle Float Float Float Float

area :: Shape -> Float
area (Circle _ _ r) = pi * r * r
area (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

-- Recursive Data Types:
data Nat =
  Zero
  | Succ Nat
  deriving (Eq, Show)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

natPlus :: Nat -> Nat -> Nat
natPlus Zero n = n
natPlus (Succ m) n = Succ (natPlus m n)


-- CYPHERS

type Code = [(Char, Char)]

-- | Shift cypher generation
shiftCode :: Int -> String -> Code
shiftCode n = map (\x -> (x, shift n x))
  where
    shift :: Int -> Char -> Char
    shift 0 c = c
    shift x 'z' = shift (x-1) 'a'
    shift x c
      | isUpper c = toUpper $ shift x (toLower c)
      | otherwise = shift (x-1) (succ c)

code :: Code
code = zip ['a'..'z'] cypher ++ zip ['A'..'Z'] (map toUpper cypher)
  where
    cypher = "thequickbrownfxjmpsvlazydg"

revcode :: Code
revcode = zip cypher alphabet ++ zip (map toUpper cypher) (map toUpper alphabet)
  where
    cypher = "thequickbrownfxjmpsvlazydg"
    alphabet = ['a'..'z']

-- lookup
lookup :: Char -> Code -> Maybe Char
lookup _ [] = Nothing
lookup c ((x, y):xs)
  | c == x = Just y
  | otherwise = lookup c xs

encode, decode :: Char -> Char
encode c = fromMaybe c (lookup c code)
decode c = fromMaybe c (lookup c revcode)

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y

encodeLine, decodeLine :: String -> String
encodeLine = map encode
decodeLine = map decode

encodeFile, decodeFile :: String -> String
encodeFile = unlines . reverse . map encodeLine . lines
decodeFile = unlines . reverse . map decodeLine . lines

processFile :: FilePath -> IO ()
processFile f = do
  contents <- readFile f
  writeFile (f ++ ".enc") (encodeFile contents)
  putStrLn "Encoding Done!"

  encodings <- readFile (f ++ ".enc")
  writeFile (f ++ ".dec") (decodeFile encodings)
  putStrLn "Decoding Done!"
