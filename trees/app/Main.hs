module Main where

import BinTree ( BinTree(..), isEmpty )

main :: IO ()
main = do
  let tree1 = Node Empty 1 Empty

  let myTree = Node (Node Empty 2 Empty) 1 (Node Empty 3 Empty)
  print myTree
  -- print "Hello!"
