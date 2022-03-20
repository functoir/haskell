{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where

import Prelude hiding (lookup, Maybe, Nothing, Just, Either, Left, Right)

import Data.Char ( isUpper, toLower, toUpper )      -- Character operations
-- import Data.Maybe hiding (fromMaybe)     -- Maybe operations

main :: IO ()
main = do
  let day1 = Monday
  print $ show day1
  print $ show $ nextDay day1
  print $ show (Monday > Tuesday)

  let weekDays = [Monday .. Friday]
  let weekEnds = [Saturday, Sunday]

  print weekDays
  print $ mergesort weekDays
  print weekEnds

  let revWeekDays = [Friday, Thursday .. Monday]
  print revWeekDays
  print $ mergesort revWeekDays
  print $ quicksort revWeekDays
  print $ mergesort [Monday]
  print $ quicksort [Monday]
  print [Monday, Wednesday .. Sunday]
  let firstDay = minBound :: Day
  let lastDay = maxBound :: Day
  print $ "first: " ++ show firstDay ++ " last: " ++ show lastDay


  -- let zero = Zero
  -- let num1 = Succ zero
  -- let num2 = Succ num1

  -- print $ show zero
  -- print $ show num1
  -- print $ show num2

  -- let num4 = num1 `natPlus` num2
  -- print $ show num4 ++ "= " ++ show (natToInt num4)

  ---- cyphers
  -- putStrLn "What file should I encode and decode?"
  -- filename <- getLine
  -- processFile filename

  -- printCode code

  ---- Polymorphic types
  -- print $ safeDiv 4832934321 4154252
  -- print $ safeDiv 4832934321 0

  -- -- Trees
  -- let tree1 = exTree :: Tree Int
  -- print tree1
  -- let tree2 = treeIncr tree1
  -- print tree2
  -- print tree1
  -- print $ "pre-order: " ++ show (preOrder exTree)
  -- print $ "in-order: " ++ show (inOrder exTree)
  -- print $ "post-order: " ++ show (postOrder exTree)
  -- print $ "size: " ++ show (size exTree)
  -- print $ "[Tree]: " ++ show exTree
  -- let list1 = convert tree1 :: [Int]
  -- print $ "[List 1]: " ++ show list1

  -- let list2 = convert tree2 :: [Int]
  -- print $ "[List 2]: " ++ show list2

  -- let ints = [1..5] ++ [5,4..1]
  -- print $ "[List 3]: " ++ show ints
  -- print $ "Sorted List 3" ++ show (mergesort ints)


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

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort arr = merge (mergesort left) (mergesort right)
    where
      (left, right) = splitAt (length arr `div` 2) arr

      merge :: Ord a => [a] -> [a] -> [a]
      merge [] ys = ys    -- [x] =========> (x:[])
      merge xs [] = xs
      merge (x:xs) (y:ys)
        | x < y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =                --(x:[])
  smallerSorted ++ [x] ++ biggerSorted
  where
    smallerSorted = quicksort [y | y <- xs, y <= x]
    biggerSorted = quicksort [y | y <- xs, y > x]

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

----------------------------------------------------------------
-- CYPHERS
----------------------------------------------------------------

-- `type` is just an alias, whereas `data` is a representational type.
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

printCode :: Code -> IO ()
printCode [] = return ()
printCode ((orig, sub):xs) = do
  print ([orig] ++ " -> " ++ [sub])
  printCode xs

----------------------------------------------------------------
-- POLYMORPHIC TYPES
----------------------------------------------------------------

data Maybe a = Nothing | Just a
  deriving (Eq, Show)

data Either a b = Left a | Right b
  deriving (Eq)

instance (Show a, Show b) => Show (Either a b) where
  show (Left a) = show a
  show (Right b) = show b

safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "Division by zero"
safeDiv x y = Right (x `div` y)

data PrimaryColor = Red | Green | Blue
  deriving (Show)

instance Eq PrimaryColor where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False


----------------------------------------------------------------
-- TREES
----------------------------------------------------------------

class Convertible a b where
  convert :: a -> b

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show, Read)

instance Convertible (Tree a) Int where
  convert = size

instance Convertible (Tree a) [a] where
  convert = inOrder

-- instance Convertible [a] Tree a where
--   convert [] = Leaf
--   convert [x] = Branch x
--   convert arr = Branch val (convert left) (convert right)
--     where
--       mid = length arr `div` 2
--       val = arr !! mid
--       left = take (mid - 1) arr
--       right = drop mid arr

instance Show a => Convertible (Tree a) [Char] where
  convert = show

instance Eq a => Eq (Tree a) where
  Leaf == Leaf = True
  (Branch x l1 r1) == (Branch y l2 r2) =
    dataEq && leftEq && rightEq
    where
      dataEq = x == y
      leftEq = l1 == l2
      rightEq = r1 == r2
  _ == _ = False

instance Ord a => Ord (Tree a) where
  (Branch x l1 r1) <= (Branch y l2 r2) =
    dataLE && leftLE && rightLE
    where
      dataLE = x <= y
      leftLE = l1 <= l2
      rightLE = r1 <= r2
  Leaf <= _ = True
  _ <= Leaf = False

instance Functor Tree where
  fmap = treeMap


size :: Tree a -> Int
size Leaf = 0
size (Branch _ l r) = 1 + size l + size r

exTree :: Tree Int
exTree =
  Branch 4
    (
      Branch 2
      (
        Branch 1 Leaf Leaf
      )
      (
        Branch 3 Leaf Leaf
      )
    )
    (
      Branch 6 (
        Branch 5 Leaf Leaf
      ) (
        Branch 7 Leaf Leaf
      )
    )

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Branch x l r) =
  Branch (f x) (treeMap f l) (treeMap f r)

treePlus :: Num b => b -> Tree b -> Tree b
treePlus x = treeMap (+ x)

treeIncr :: (Num a ) => Tree a -> Tree a
treeIncr = treePlus 1

treeFold :: (a -> b -> b -> b) -> b -> Tree a -> b
treeFold _ acc Leaf = acc
treeFold f acc (Branch x l r) = f x (treeFold f acc l) (treeFold f acc r)

preOrder, inOrder, postOrder :: Tree a -> [a]
preOrder = treeFold (\x l r -> [x] ++ l ++ r) []
inOrder = treeFold (\x l r -> l ++ [x] ++ r) []
postOrder = treeFold (\x l r -> l ++ r ++ [x]) []

toList :: Tree a -> [a]
toList = inOrder
