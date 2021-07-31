import GHC.Types (Any, RuntimeRep (AddrRep))

-- ! Generate an ascending list of integers in range
asc :: (Ord t, Num t) => t -> t -> [t]
asc lo hi
  | lo > hi = []
  | lo == hi = [hi]
  | otherwise = lo : asc (lo + 1) hi




-- !01: Function that returns True if element occurs in a List

-- | V1 using manual folding
contains :: Eq t => t -> [t] -> Bool
contains _ [] = False
contains num (elem : arr) =
  (num == elem) || contains num arr

-- | V2 using foldr
containsV2 :: (Foldable t, Eq a) => a -> t a -> Bool
containsV2 num arr = foldr (\ elem -> (||) (num == elem)) False arr

-- | V3 using foldr (optimized)
containsV3 :: (Foldable t, Eq a) => a -> t a -> Bool
containsV3 num = foldr (\ elem -> (||) (num == elem)) False




-- ! 02 Remove duplicates from a list

-- nub -- original solution
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = helper x acc
  where
    acc = nub xs
    helper x acc
      | contains x acc = acc
      | otherwise = x:acc

-- nub2: no use of helper
nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x:xs)
  | x `elem` xs = nub2 xs
  | otherwise = x : nub2 xs


-- ! 03: Checking lists for ascending ordering

first :: Num p => [p] -> p
first [] = 1000
first (x:xs) = x


isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x:y:xs) = 
  (x <= y) && isAsc (y:xs)


-- ! 02 Directed Paths in Graphs -- Depth First Search

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] u v = u == v                          -- Empty Graph cannot contain path except node to itself
hasPath arr u v
  | u == v = True
  | otherwise = 
    let xs' = [ (n,m) | (n,m) <- arr, n /= u] in
    or [hasPath xs' m v | (n,m) <- arr, n == u]
  
  




main :: IO ()
main = do
  let myList = asc 3 10
  print " -- -- -- Problem 01 -- -- -- "
  print (contains 7 myList)
  print (contains 14 myList)
  print (containsV2 7 myList)
  print (containsV2 14 myList)
  print (containsV3 7 myList)
  print (containsV3 14 myList)

  print " -- -- -- Problem 02 -- -- -- "
  let myList2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6]
  putStr "original list: "
  putStr (show myList2)
  putStr " -> no duplicates: "
  print (nub myList2)

  print " -- -- -- Problem 03 -- -- -- "
  let myList3 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  putStr "original list: "
  putStr (show myList3)
  putStr " -> is ascending? "
  print (isAsc myList3)


  print " -- -- -- Problem 04 -- -- -- "
  let myList4 = [(1, 2), (2, 4), (3, 4), (4, 1)]
  putStr "Graph: "
  putStr (show myList4)
  putStr " -> Path from 1 to 4? "
  print (hasPath myList4 1 4)
  putStr "Graph: "
  putStr (show myList4)
  putStr " -> Path from 1 to 3? "
  print (hasPath myList4 1 3)
