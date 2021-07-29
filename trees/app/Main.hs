module Main where

import BinTree

main :: IO ()
main = do

  let tree2 = buildTree [1,2,3,4,5,6,7,8,9,10]

  let myTree = Node (Node Empty 2 Empty) 1 (Node Empty 3 Empty)
  print myTree
  print tree2

  let tree3 = buildTree ['a', 'b', 'c', 'd', 'e', 'f']
  print tree3
  print $ "size = " ++ show(size tree3)

  let tree4 = Node Empty 5 Empty
  print $! insert 2 $! insert 4 $! insert 3 tree4
  print $! insert 3 tree4

  print tree4

  print $ tree4 == tree4
  print $ tree4 /= tree4



