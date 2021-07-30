module Main where

import BinTree

main :: IO ()
main = do

  let tree0 = buildTree [0, 1, 2, 3, 4, 5]
  print tree0
  print $ "min element = " ++ show (minElem tree0)
  print $ "max element = " ++ show (maxElem tree0)

  putStr "\n\n"
  print $ delete 2 tree0
  putStr "\n\n"
  print $ delete 3 tree0

  let tree2 = buildTree [1,2,3,4,5,6,7,8,9,10]
  putStr "\n\n"

  let myTree = Node (Node Empty 2 Empty) 1 (Node Empty 3 Empty)
  print myTree
  print tree2

  putStr "\n\n"

  let tree3 = buildTree ['a', 'b', 'c', 'd', 'e', 'f']
  print tree3
  print $ "size = " ++ show(size tree3)
  print $ delete 'c' tree3
  putStr "\n\n"

  let tree4 = Node Empty 5 Empty
  print $! insert 2 $! insert 4 $! insert 3 tree4
  print $! insert 3 tree4
  print tree4
  print $ tree4 == tree4
  print $ tree4 /= tree4

  putStr "\n\n"
  let tree5 = buildTree [1,2,3,4,5,6,7,8,9,10]
  print tree5

  let newTree = insert (-10) (insert 21 tree5)
  print newTree
  print $ "array = " ++ show (toArray newTree)
  print $! size newTree
  print $! "sum = " ++ show (sumTree newTree)
  print $! "height = " ++ show (height newTree)
  putStr "\n\n"

  let tree6 = buildTree [1, 2, 3, 4, 5]
  print $! sumTree tree6
  print $ "height = " ++ show (height tree6)


