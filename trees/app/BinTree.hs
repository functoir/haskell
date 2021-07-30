-- {-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-- | 
  Module implementing a Binary Tree in Haskell.
  Exported functions include: 
    `BinTree (Empty, Node),
    isEmpty,
    buildTree, toArray,
    contains, insert, delete,
    isLT, isEQ, isGT,
    size, height, sumTree,
    minElem, maxElem`
--} 
module BinTree (
  BinTree (Empty, Node),
  isEmpty,
  buildTree, toArray,
  contains, insert, delete,
  isLT, isEQ, isGT,
  size, height, sumTree,
  minElem, maxElem
) where

import Prelude

data BinTree a =
  Empty -- ^ Empty constructor
  |
  Node -- ^ Loaded constructor
  {
    left :: BinTree a,  -- ^ left node
    val :: a,           -- ^ data
    right :: BinTree a  -- ^ right node
  }

-- | Check equality of two trees, including a recursive check of their left and right children.
instance (Eq a, Ord a) => Eq (BinTree a) where
  (==) a b = check a b True
    where
      check t1 t2 status
        | not status = status
        | isEmpty t1 && isEmpty t2 = True
        | isEmpty t1 || isEmpty t2 = False
        | otherwise = check (left t1) (left t2) $! (check (right t1) (right t2) $! (val t1 == val t2))

-- | Generate an ordering of two tree nodes.
instance Ord a => Ord (BinTree a) where
  compare t1 t2
    | isEmpty t1 && isEmpty t2 = EQ
    | isEmpty t1 = GT
    | isEmpty t2 = LT
    | otherwise = compare (val t1) (val t2)


-- | Generate a string representation of a `BinTree`.
instance Show a => Show (BinTree a) where
  show t
    | isEmpty t = "Empty tree."
    | otherwise = unlines $ build t
      where
        build tree
          | isEmpty tree = []
          | otherwise =
            show (val tree) : buildSub (left tree) (right tree)
        buildSub l r = (pad "+- " "|  " $! build r) ++ (pad "`- " "|  " $! build l)
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
  | otherwise = val node == item

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

-- | Compute the size of a tree
size :: BinTree a -> Int
size = treeFold (\_ acc -> acc + 1) 0

-- | Compute the height of a tree.
height :: BinTree a -> Int
height node
  | isEmpty node = 0
  | otherwise = 1 + max (height (left node)) (height (right node))

-- | Compute the sum of all items in tree. Must be Numeric data.
sumTree :: Num a => BinTree a -> a
sumTree = treeFold (+) 0

-- | Check whether a`BinTree` instance contains given value.
contains :: (Eq a, Ord a) => a -> BinTree a -> Bool
contains item node
  | isEmpty node = False
  | isEQ item node = True
  | isLT item node = contains item (right node)
  | isGT item node = contains item (left node)
  | otherwise = False

-- | Get the smallest item in the binary tree.
minElem :: (Ord a) => BinTree a -> a
minElem node
  | isEmpty (left node) = val node
  | otherwise = minElem (left node)

-- | Get the largest element in the binary tree.
maxElem :: (Ord a) => BinTree a -> a
maxElem node
  | isEmpty (right node) = val node
  | otherwise = maxElem (right node)

-- ! Modify data
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
  | isEmpty node = Empty
  | isLT item node = Node (left node) (val node) (delete item (right node))
  | isGT item node = Node (delete item (left node)) (val node) (right node)
  | otherwise = delRoot node
    where
      delRoot :: Ord a => BinTree a -> BinTree a
      delRoot t
        | isEmpty (left t) && isEmpty (right t) = Empty
        | isEmpty $ left t = right t
        | isEmpty $ right t = left t
        | otherwise = let successor = minElem $ right t in
          Node (left t) successor (delete successor (right t))

-- ! folding

-- | fold a function over a tree.
treeFold :: (a -> b -> b) -> b -> BinTree a -> b
treeFold _ acc Empty = acc
treeFold f acc (Node l v r) = treeFold f (treeFold f (f v acc ) l) r


-- ! Tree to Array & Vuce versa

-- | flatten a tree into an array
toArray :: (Ord a) => BinTree a -> [a]
toArray node = build node []
  where
    build Empty arr = arr
    build (Node l v r) arr = build l $! v : build r arr

-- | Construct a balanced Binary tree from a list of values.
buildTree :: (Eq a, Ord a) => [a] -> BinTree a
buildTree arr = constr Empty (qsort arr)
  where
    constr :: (Ord a) => BinTree a -> [a] -> BinTree a
    constr t [] = t
    constr t array =
      Node (constr (left inserted) (smallerElements item array))
       (val inserted)
        (constr (right inserted) (biggerElements item array))
        where
          inserted = insert item t
          item = array !! ((length array - 1) `div` 2)

{- ARRAY HELPERS -}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort arr = build arr []
  where
    build [] acc = acc
    build (x:xs) acc = build lXS (x : build rXS acc)
      where 
        lXS = [y | y <- xs, y < x]
        rXS = [y | y <- xs, y >= x]

-- | Filter out smaller elements in an array.
smallerElements :: Ord a => a -> [a] -> [a]
smallerElements _ [] = []
smallerElements n (x:xs)
  | x < n = x : smallerElements n xs
  | otherwise = smallerElements n xs

-- | Filter out bigger elements in an array.
biggerElements :: Ord a => a -> [a] -> [a]
biggerElements _ [] = []
biggerElements n (x:xs)
  | x > n = x : biggerElements n xs
  | otherwise = biggerElements n xs
