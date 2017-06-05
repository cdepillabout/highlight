{-# LANGUAGE OverloadedStrings #-}

module Highlight.Options where

import Control.Applicative (many)
import Data.Monoid ((<>))
import Data.Text (Text)
import Options.Applicative
       (Parser, flag, help, long, metavar, short, strArgument)

data IgnoreCase = IgnoreCase | DoNotIgnoreCase

data Recursive = Recursive | NotRecursive

data ColorGrepFilenames = ColorGrepFilenames | DoNotColorGrepFileNames

data RegEx = RegEx Text

data InputFilename = InputFilename FilePath

data Options = Options
  { optionsIgnoreCase :: IgnoreCase
  , optionsRecursive :: Recursive
  , optionsColorGrepFilenames :: ColorGrepFilenames
  , optionsRegEx :: RegEx
  , optionsInputFilenames :: [InputFilename]
  }


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
regExParser = pure $ RegEx "what"

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
