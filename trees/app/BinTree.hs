-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module BinTree (
  BinTree (Empty, Node),
  isEmpty,
  buildTree,
  contains, insert, delete,
  isLT, isEQ, isGT, size
) where

import qualified Data.List
import Unsafe.Coerce ( unsafeCoerce )

data BinTree a =
  Empty -- ^ Empty constructor
  |
  Node -- ^ Loaded constructor
  {
    left :: BinTree a,  -- ^ left node
    val :: a,           -- ^ data
    right :: BinTree a  -- ^ right node
  }

instance (Eq a, Ord a) => Eq (BinTree a) where
  (==) a b = check a b True
    where
      check t1 t2 status
        | not status = False
        | isEmpty t1 && isEmpty t2 = True
        | isEmpty t1 || isEmpty t2 = False
        | otherwise = check (left t1) (left t2) $! (check (right t1) (right t2) $! (val t1 == val t2))
  (/=) a b = not (a == b)

instance Ord a => Ord (BinTree a) where
  compare t1 t2
    | isEmpty t1 && isEmpty t2 = EQ
    | isEmpty t1 = GT
    | isEmpty t2 = LT
    | otherwise = compare (val t1) (val t2)


instance Show a => Show (BinTree a) where
  show t
    | isEmpty t = "Empty tree."
    | otherwise = unlines $ build t
      where
        build tree
          | isEmpty tree = []
          | otherwise =
            show (val tree) : buildSub (left tree) (right tree)
            where
              buildSub l r =
                (pad "+- " "|  " $! build r) ++ (pad "`- " "| " $! build l)
                  where
                    pad first rest = zipWith (++) (first : repeat rest)

{--! Check Information -}
-- | Check whether a given `BinTree` instance is the `Empty` constructor.
isEmpty :: BinTree a -> Bool
isEmpty Empty = True
isEmpty   _   = False

-- | Check whether an item is equal to the item in current `Node`.
isEQ :: Eq a => a -> BinTree a -> Bool
isEQ item node
  | isEmpty node = False
  | otherwise = item == val node

-- | Check whether item in `Node` is less than given item.
isLT :: Ord a => a -> BinTree a -> Bool
isLT item node
  | isEmpty node = False
  | otherwise = val node < item

-- | Check whether item in `Node` is greater than given item.
isGT :: Ord a => a -> BinTree a -> Bool
isGT item node
  | isEmpty node = False
  | otherwise = val node > item

size :: BinTree a -> Int
size node = check node 0
    where
      check Empty count = count
      check node count = check (left node)  $! check (right node) $! count + 1

-- | Check whether a`BinTree` instance contains given value.
contains :: (Eq a, Ord a) => a -> BinTree a -> Bool
contains item node
  | isEmpty node = False
  | isEQ item node = True
  | isLT item node = contains item (right node)
  | isGT item node = contains item (left node)
  | otherwise = False

-- | Percolate a value down a `BinTree` and insert in an appropriate position.
insert :: (Eq a, Ord a) => a -> BinTree a -> BinTree a
insert item node
  | isEmpty node = Node Empty item Empty
  | isEQ item node = node
  | isLT item node = Node (left node) (val node) (insert item (right node))
  | isGT item node = Node (insert item (left node)) (val node) (right node)
  | otherwise = node

-- | Delete value from a `BinTree` instance.
delete :: (Eq a, Ord a) => a -> BinTree a -> BinTree a
delete item node
  | isEmpty node = node
  | isLT item node = delete item (left node)
  | isGT item node = delete item (right node)
  | isEQ item node = delRoot node
  | otherwise = node   -- !!! Handle outlier cases.


delRoot :: Ord a => BinTree a -> BinTree a
delRoot t
  | isEmpty (left t) = right t
  | isEmpty (right t) = left t
  | otherwise = let v2 = leftmost (right t) in
      Node (left t) v2 (delete v2 (right t))
        where
          leftmost :: (Ord a) => BinTree a -> a
          leftmost node
            | isEmpty (left node) = val node
            | otherwise = leftmost (left node)
          {-
            OK to ignore the Empty constructor, since:
            (a) we do not export this function and 
            (b)we do not call it with the Empty constructor.
          -}

-- | Construct a balanced Binary tree from a list of values.
buildTree :: (Eq a, Ord a) => [a] -> BinTree a
buildTree arr = constr Empty (quicksort arr)
  where
    constr :: (Ord a) => BinTree a -> [a] -> BinTree a
    constr t [] = t
    constr t arr =
      Node (constr (left inserted) (smallerElements item arr)) (val inserted) (constr (right inserted) (biggerElements item arr))
        where
          inserted = insert item t
          item = midElem arr
          midElem arr = arr !! (length arr `div` 2)

{- ARRAY HELPERS -}
-- | quicksort an array
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

-- | Delete first occurence of an element from an array.
delFromArr :: (Eq a) => a -> [a] -> [a]
delFromArr n arr = removeFirst n arr []
  where
    removeFirst :: (Eq a) => a -> [a] -> [a] -> [a]
    removeFirst num arr acc
      | null arr = acc
      | head arr == num = acc ++ tail arr
      | otherwise = removeFirst num (tail arr) (acc ++ [head arr])

-- | Filter out smaller elements in an array.
smallerElements :: Ord a => a -> [a] -> [a]
smallerElements n [] = []
smallerElements n (x:xs)
  | x < n = x : smallerElements n xs
  | otherwise = smallerElements n xs

-- | Filter out bigger elements in an array.
biggerElements :: Ord a => a -> [a] -> [a]
biggerElements n [] = []
biggerElements n (x:xs)
  | x > n = x : biggerElements n xs
  | otherwise = biggerElements n xs

