module Main where

import System.Environment ( getArgs, getProgName, lookupEnv )
import System.Exit ( exitSuccess, exitFailure )

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " [-h | --help | -v | --version] <greeting>"

printVersion :: IO ()
printVersion = putStrLn "Version 01"

mainAct :: [String] -> IO ()
mainAct [] = do
  putStrLn "Please provide a greeting."
  printHelp
  exitFailure
  
mainAct args = do
  let greeting = unwords args
  name <- lookupEnv "USER"
  putStrLn $ maybe "No user to greet!" (\name -> greeting ++ " " ++ name) name


main :: IO ()
main = do
  args <- getArgs
  if "-h" `elem` args || "--help" `elem` args then
    printHelp >> exitSuccess
  else if "-v" `elem` args || "--version" `elem` args then
    printVersion >> exitSuccess
  else
    mainAct args >> exitSuccess



  -- dummies
  -- name <- getProgName
  -- args <- getArgs
  -- user <- lookupEnv "USER"
  -- editor <- lookupEnv "EDITOR"
  -- shell <- lookupEnv "SHELL"
  -- print $ "prog name : " ++ show name
  -- print $ "commandline arguments : " ++ show args
  -- print $ "user : " ++ show user
  -- print $ "editor : " ++ show editor
  -- print $ "shell : " ++ show shell
  -- exitSuccess