{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module BinTree (
  BinTree (Empty, Node), isEmpty
) where

import qualified Data.List

data (Eq a, Ord a) => BinTree a =
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
        | otherwise = check (left t1) (left t2) (check (right t1) (right t2) (val t1 == val t2))

  (/=) a b = not (a == b)

instance Ord a => Ord (BinTree a) where
  compare t1 t2
    | isEmpty t1 && isEmpty t2 = EQ
    | isEmpty t1 = GT
    | isEmpty t2 = LT
    | otherwise = compare (val t1) (val t2)

instance (Eq a, Ord a, Show a) => Show (BinTree a) where
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
                pad "+- " "|  " (build r) ++ pad "+- " "| " (build l)
                  where
                    pad first rest = zipWith (++) (first : repeat rest)

{- Check Information -}
-- | Check whether a given `BinTree` instance is the `Empty` constructor.
isEmpty :: Eq a => BinTree a -> Bool
isEmpty Empty = True
isEmpty   _   = False

-- | Check whether a`BinTree` instance contains given value.
contains :: (Eq a, Ord a) => a -> BinTree a -> Bool
contains item node
  | isEmpty node = False
  | val node == item = True
  | val node < item = contains item (right node)
  | val node > item = contains item (left node)
  | otherwise = False

-- | Percolate a value down a `BinTree` and insert in an appropriate position.
insert :: (Eq a, Ord a) => a -> BinTree a -> BinTree a
insert item node
  | isEmpty node = Node Empty item Empty
  | val node == item = node
  | val node < item = Node (left node) (val node) (insert item (right node))
  | otherwise = Node (insert item (left node)) (val node) (right node)

-- | Delete value from a `BinTree` instance.
delete :: (Eq a, Ord a) => BinTree a -> a -> BinTree a
delete t x
  | isEmpty t = t
  | val t > x = delete (left t) x
  | val t > x = delete (right t) x
  | val t == x = delRoot t
  | otherwise = t


delRoot :: Ord a => BinTree a -> BinTree a
delRoot t
  | isEmpty (left t) = right t
  | isEmpty (right t) = left t
  | otherwise = let v2 = leftmost (right t) in
      Node (left t) v2 (delete (right t) v2)
        where
          leftmost :: BinTree a -> a
          leftmost (Node Empty v _) = v
          leftmost (Node l _ _) = leftmost l

buildTree :: (Eq a, Ord a) => [a] -> BinTree a
buildTree arr = constr Empty (quicksort arr)
  where
    constr :: BinTree a -> [a] -> BinTree a
    constr t [] = t
    constr t arr =
      let midElem = arr !! (length arr `div` 2) in
      constr (insert midElem t) (delFromArr midElem arr)

{- ARRAY HELPERS -}
-- | quicksort an array
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

delFromArr :: (Eq a) => a -> [a] -> [a]
delFromArr n arr = removeFirst n arr []
  where
    removeFirst :: (Eq a) => a -> [a] -> [a] -> [a]
    removeFirst num arr acc
      | null arr = acc
      | head arr == num = acc ++ tail arr
      | otherwise = removeFirst num (tail arr) (acc ++ [head arr])