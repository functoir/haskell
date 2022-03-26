{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sudoku (
  solve,
  solution
, valid, expand, solve') where


import Data.List ((\\))
import Prelude hiding ()

type Digit = Char
type Row a = [a]
type Matrix a = [Row a]
type Grid = Matrix Digit
type Choices = [Digit]

digits :: [Digit]
digits = "123456789"

blank :: Digit -> Bool
blank = (== '-')

solve' :: Grid -> [Grid]
solve' = filter valid . expand . prune . choices

solve :: Grid -> [Grid]
solve = search . prune . choices

choice :: Digit -> [Digit]
choice d
  | blank d = digits
  | otherwise = [d]

solution :: Grid -> Grid
solution = head . solve

choices :: Grid -> Matrix Choices
choices = map (map choice)

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

expand :: Matrix Choices -> [Grid]
expand = cp . map cp

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

rows :: a -> a
rows = id
cols :: [[a]] -> [[a]]
cols [] = []
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group
  where
    group :: [a] -> [[a]]
    group xs = take 3 xs : group (drop 3 xs)
    ungroup :: [[a]] -> [a]
    ungroup = concat

valid :: Grid -> Bool
valid g =
  all nodups (rows g) &&
  all nodups (cols g) &&
  all nodups (boxs g)

prune :: [[[Digit]]] -> [[[Digit]]]
prune =
  pruneby boxs .
  pruneby cols .
  pruneby rows
  where
    pruneby f = map pruneRow . f
    pruneRow row = map (remove fixed) row
      where
        fixed = [d | [d] <- row]

    remove xs ds
      | length ds == 1 = ds
      | otherwise = ds \\ xs

expand1 :: Matrix Choices -> [Matrix Choices]
expand1 rows =
  [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
    where
      (rows1, row:rows2) = break (any smallest) rows
      (row1, cs:row2) = break smallest row
      smallest cs = length cs == n
      n = minimum (counts rows)
      counts = filter (/=1) . map length . concat

safe :: Matrix Choices -> Bool
safe m =
  all ok (rows m) &&
  all ok (cols m) &&
  all ok (boxs m)
    where
      ok row = nodups [d | [d] <- row]

complete :: Matrix Choices -> Bool
complete = all (all singleton)
  where
    singleton l = length l == 1

search :: Matrix Choices -> [Grid]
search m
  | not (safe m') = []
  | complete m' = [map (map head) m']
  | otherwise = concatMap search $ expand1 m'
    where
      m' = prune m
