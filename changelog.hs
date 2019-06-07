#!/usr/bin/env stack
-- stack --resolver lts-13.16 script

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Applicative          ((<|>))
import           Control.Monad
import qualified Data.ByteString              as BS
import           Data.Char
import           Data.List
import qualified Data.Yaml                    as Y
import           GHC.Generics
import           System.Directory
import           System.Environment
import           System.Exit                  hiding (die)
import           System.FilePath.Posix        ((</>))
import           Text.ParserCombinators.ReadP


data UnreleasedEntry
  = UnreleasedEntry
      { title        :: Maybe String
      , mergeRequest :: Maybe String
      , story        :: Maybe String
      , entryType    :: Maybe String
      , instructions :: Maybe String
      } deriving (Generic, Show)
instance Y.FromJSON UnreleasedEntry

data Command
   = Release { versionNumber :: VersionNumber }
   | Version
   | Help deriving (Show)

data CommandError
   = NoVersionSpecified
   | UnknownCommand String deriving (Show)

data ParseError
  = BadYaml FilePath deriving (Show)

type Major = Int
type Minor = Int
type Patch = Int
type VersionNumber = (Major, Minor, Patch, Maybe String)

main :: IO ()
main = do
  args <- getArgs

  case parse args of
    Right command -> handleCommand command
    Left error    -> handleError error >> die

parse :: [String] -> Either CommandError Command
parse [] = Right Help
parse (arg:args) =
  case map toLower arg of
    "release"      -> parseReleaseArgs args
    "help"         -> Right Help
    "version"      -> Right Version
    unknownCommand -> Left $ UnknownCommand unknownCommand

parseReleaseArgs :: [String] -> Either CommandError Command
parseReleaseArgs [] = Left NoVersionSpecified
parseReleaseArgs (arg:_) =
  case parseVersionNumber arg of
    Just version -> Right $ Release { versionNumber = version }
    Nothing      -> Left NoVersionSpecified
  where
    number :: ReadP Int
    number = fmap read (many1 $ satisfy isDigit)
    letters :: ReadP String
    letters = fmap read (many1 $ satisfy isLetter)
    dot :: ReadP Char
    dot = satisfy (== '.')
    versionParser :: ReadP VersionNumber
    versionParser = do
      major <- number; dot
      minor <- number; dot
      patch <- number;
      build <- option Nothing (fmap Just versionBuildParser)
      return (major, minor, patch, build)
    versionBuildParser :: ReadP String
    versionBuildParser = do
      dot; letters
    parseVersionNumber :: String -> Maybe VersionNumber
    parseVersionNumber arg =
      case readP_to_S versionParser arg of
        []             -> Nothing
        ((result,_):_) -> Just result

unreleasedDirectory :: FilePath
unreleasedDirectory =
  "changelogs" </> "unreleased"

processRelease :: VersionNumber -> IO ()
processRelease version = do
  unreleasedEntries <- getUnreleasedEntries
  case unreleasedEntries of
    Left (BadYaml file) -> (putStrLn $ "Invalid YAML in '" ++ file ++ "'") >> die
    Right entries       -> (putStrLn $ show entries) >> exit

getUnreleasedEntries :: IO (Either ParseError [UnreleasedEntry])
getUnreleasedEntries = do
  files <- getUnreleasedFiles
  unreleasedEntries <- mapM Y.decodeFileEither files
  return $ foldl checkFiles (Right []) $ zip files unreleasedEntries
  where
    getUnreleasedFiles :: IO [FilePath]
    getUnreleasedFiles =
      liftM onlyYamlFiles $ getDirectoryContents unreleasedDirectory
    onlyYamlFiles :: [[Char]] -> [FilePath]
    onlyYamlFiles =
      map (unreleasedDirectory </>) . filter (isSuffixOf ".yaml")
    checkFiles ::  Either ParseError [UnreleasedEntry] -> (FilePath, Either Y.ParseException UnreleasedEntry) -> Either ParseError [UnreleasedEntry]
    checkFiles (Left error) _ = Left error
    checkFiles (Right entries) (file, possibleEntry) =
      case possibleEntry of
        Left _      -> Left $ BadYaml file
        Right entry -> Right $ entry:entries

handleCommand :: Command -> IO ()
handleCommand Help          = showUsage >> exit
handleCommand Version       = showVersion >> exit
handleCommand (Release rel) = processRelease rel

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
    \  release\n\
    \  help\n\
    \  version\n\n\
    \More documentation can be found at https://github.com/tuxagon/changelog-haskell"

showVersion :: IO ()
showVersion = putStrLn "changelog 0.1"

exit :: IO ()
exit = exitWith ExitSuccess

die :: IO ()
die = exitWith (ExitFailure 1)
