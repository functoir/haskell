{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where
import Prelude

import Sudoku( solve, solution)

main :: IO ()
main = do
  putStrLn "Started"
  -- print $ solve ["-52439817", "8-9165432", "41-872596", "548-97321", "9315-4768", "26738-945", "795213-84", "1849562-3", "32674815-"]
  print $ solution ["-5-43-81-", "-------3-", "-13--2---", "--8-9---1", "9-15-4-68", "-67---945", "795----84", "-8-956---", "32-748-59"]
