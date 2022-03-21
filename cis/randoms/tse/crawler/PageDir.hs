{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module PageDir ( 
  pagedirInit,
  pagedirSave
) where

import Prelude

import System.FilePath.Posix (FilePath) 


-- pagedirInit :: FilePath -> Bool
pagedirInit [] = False
pagedirInit dir = do
  let filename = dir </> ".crawler"
  writeFile filename "" >> True


  
