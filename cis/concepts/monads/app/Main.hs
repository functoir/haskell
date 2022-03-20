{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where

import Prelude hiding (filter)
-- import Data.Char (toUpper)
-- import Control.Monad (liftM, liftM2, guard)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  let tree1 = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))
  let tree2 = Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))
  let tree3 = Leaf "Single String"

  print tree1
  print tree2
  print $ zipTree tree1 tree2
  print $ zipTree tree1 $ zipTree tree1 tree2
  print $ zipTree tree1 tree3


-- | Tree with values at leaves.
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

zipTree :: Tree a -> Tree b -> Tree (a, b)
zipTree (Leaf a) (Leaf b) = Leaf (a, b)
zipTree (Branch l1 r1) (Branch l2 r2) =
  Branch (zipTree l1 l2) (zipTree r1 r2)
zipTree _ _ = error "Incompatible Zip."

