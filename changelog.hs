#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Data.Char
import           System.Environment
import           System.Exit        hiding (die)


data Command
   = Help
   | Version
   | Unknown String deriving (Show)

main = do
  args <- getArgs

  case parse args of
    Help            -> usage >> exit
    Version         -> version >> exit
    Unknown command -> unrecognized command >> usage >> die

parse :: [String] -> Command
parse [] = Help
parse (arg:args) =
  case map toLower arg of
    "help"         -> Help
    "version"      -> Version
    unknownCommand -> Unknown unknownCommand

usage   =
  putStrLn
    "changelog - Changelog Utility\n\n\
    \Available Commands:\n\
    \  help\n\
    \  version\n\n\
    \More documentation can be found at https://github.com/tuxagon/changelog-haskell"
version = putStrLn "changelog 0.1"
unrecognized command =
  putStrLn $ "Unrecognized command: '" ++ command ++ "'\n"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
