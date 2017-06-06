{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Options where

import Control.Applicative (many)
import Data.Monoid ((<>))
import Options.Applicative
       (Parser, flag, help, long, metavar, short, strArgument)
-- import Text.RE.PCRE.ByteString
--        (RE, SimpleREOptions(MultilineInsensitive), compileRegexWith,
--         reSource)

data IgnoreCase = IgnoreCase | DoNotIgnoreCase
  deriving (Eq, Read, Show)

data Recursive = Recursive | NotRecursive
  deriving (Eq, Read, Show)

data ColorGrepFilenames = ColorGrepFilenames | DoNotColorGrepFileNames
  deriving (Eq, Read, Show)

newtype RegEx = RegEx
  { unRegEx :: String
  } deriving (Eq, Read, Show)

newtype InputFilename = InputFilename
  { unInputFilename :: FilePath
  } deriving (Eq, Read, Show)

data Options = Options
  { optionsIgnoreCase :: IgnoreCase
  , optionsRecursive :: Recursive
  , optionsColorGrepFilenames :: ColorGrepFilenames
  , optionsRegEx :: RegEx
  , optionsInputFilenames :: [InputFilename]
  } deriving (Eq, Read, Show)


-- sample :: Parser Sample
-- sample =
--   Sample <$>
--   strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting") <*>
--   switch (long "quiet" <> short 'q' <> help "Whether to be quiet") <*>
--   option
--     auto
--     (long "repeat" <> help "Repeats for greeting" <> showDefault <> value 1 <>
--      metavar "INT")

ignoreCaseParser :: Parser IgnoreCase
ignoreCaseParser =
  flag
    DoNotIgnoreCase
    IgnoreCase
    (long "ignore-case" <> short 'i' <> help "ignore case distinctions")

recursiveParser :: Parser Recursive
recursiveParser =
  let mods =
        long "recursive" <>
        short 'r' <>
        help "recursive operate on files under specified directory"
  in flag NotRecursive Recursive mods

colorGrepFilenamesParser :: Parser ColorGrepFilenames
colorGrepFilenamesParser =
  let mods =
        long "from-grep" <>
        short 'g' <>
        help "highlight output from grep"
  in flag DoNotColorGrepFileNames ColorGrepFilenames mods

regExParser :: Parser RegEx
regExParser =
  let mods = metavar "PATTERN"
  in RegEx <$> strArgument mods

inputFilenamesParser :: Parser [InputFilename]
inputFilenamesParser =
  let mods = metavar "FILE"
  in many $ InputFilename <$> strArgument mods

optionsParser :: Parser Options
optionsParser =
  Options
    <$> ignoreCaseParser
    <*> recursiveParser
    <*> colorGrepFilenamesParser
    <*> regExParser
    <*> inputFilenamesParser
