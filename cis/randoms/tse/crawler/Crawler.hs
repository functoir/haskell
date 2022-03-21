{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where

import Prelude hiding ()

import System.Environment (getArgs)

import PageDir ( pagedirInit )

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  args <- getArgs
  print args
  return ()


-- parseArgs = do
--   args <- getArgs
--   let usage = "Usage: ./crawler [seedURL][pageDirectory] [maxDepth]"
--   if length args /= 4 then
--     fail "Invalid number of arguments\n" ++ usage
--   else
--     let url = normalizeURL (args !! 1)
--         dir = pagedirInit (args !! 2)
--         depth = read (args !! 3)          :: Int


  