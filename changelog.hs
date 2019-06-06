#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Applicative          ((<|>))
import           Data.Char
import           System.Environment
import           System.Exit                  hiding (die)
import           Text.ParserCombinators.ReadP

  {-

data UnreleasedEntry
  = UnreleasedEntry
      { title        :: Maybe String
      , mergeRequest :: Maybe String
      , story        :: Maybe String
      , entryType    :: Maybe String
      }

$(deriveJSON defaultOptions ''UnreleasedEntry)

unreleasedEntry :: UnreleasedEntry
unreleasedEntry =
  UnreleasedEntry
    { title = Nothing
    , mergeRequest = Nothing
    , story = Nothing
    , entryType = Nothing
    }
      -}

data Command
   = Release { versionNumber :: VersionNumber }
   | Version
   | Help deriving (Show)

data CommandError
   = NoVersionSpecified
   | UnknownCommand String deriving (Show)

type Major = Int
type Minor = Int
type Patch = Int
type VersionNumber = (Major, Minor, Patch)

main :: IO ()
main = do
  args <- getArgs

  case parse args of
    Left command -> handleCommand command
    Right error  -> handleError error >> die

parse :: [String] -> Either Command CommandError
parse [] = Left Help
parse (arg:args) =
  case map toLower arg of
    "release"      -> parseReleaseArgs args
    "help"         -> Left Help
    "version"      -> Left Version
    unknownCommand -> Right $ UnknownCommand unknownCommand

parseReleaseArgs :: [String] -> Either Command CommandError
parseReleaseArgs [] = Right NoVersionSpecified
parseReleaseArgs (arg:_) =
  case parseVersionNumber arg of
    Just version -> Left $ Release { versionNumber = version }
    Nothing      -> Right NoVersionSpecified
  where
    number :: ReadP Int
    number = fmap read (many1 $ satisfy isDigit)
    dot :: ReadP Char
    dot = satisfy (== '.')
    versionParser :: ReadP VersionNumber
    versionParser = do
      major <- number; dot
      minor <- number; dot
      patch <- number; eof
      return (major, minor, patch)
    parseVersionNumber :: String -> Maybe VersionNumber
    parseVersionNumber arg =
      case readP_to_S versionParser arg of
        []             -> Nothing
        ((result,_):_) -> Just result

processRelease :: VersionNumber -> IO ()
processRelease version =
  putStrLn $ show version

handleCommand :: Command -> IO ()
handleCommand Help          = showUsage >> exit
handleCommand Version       = showVersion >> exit
handleCommand (Release rel) = processRelease rel >> exit

handleError :: CommandError -> IO ()
handleError error =
  putStrLn $ "Error: " ++ case error of
    NoVersionSpecified     -> "Unrecognized version - Use the format X.X.X"
    UnknownCommand command -> "Unrecognized command: '" ++ command ++ "'"

showUsage :: IO ()
showUsage =
  putStrLn
    "changelog - Changelog Utility\n\n\
    \Available Commands:\n\
    \  help\n\
    \  version\n\n\
    \More documentation can be found at https://github.com/tuxagon/changelog-haskell"

showVersion :: IO ()
showVersion = putStrLn "changelog 0.1"

exit :: IO ()
exit = exitWith ExitSuccess

die :: IO ()
die = exitWith (ExitFailure 1)
