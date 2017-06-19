{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Common.Options where

import Prelude ()
import Prelude.Compat

import Control.Applicative (many)
import Data.Monoid ((<>))
import Options.Applicative
       (Parser, flag, help, long, metavar, short, strArgument)

data IgnoreCase = IgnoreCase | DoNotIgnoreCase
  deriving (Eq, Read, Show)

data Recursive = Recursive | NotRecursive
  deriving (Eq, Read, Show)

newtype RawRegex = RawRegex
  { unRawRegex :: String
  } deriving (Eq, Read, Show)

newtype InputFilename = InputFilename
  { unInputFilename :: FilePath
  } deriving (Eq, Read, Show)

data CommonOptions = CommonOptions
  { optionsIgnoreCase :: IgnoreCase
  , optionsRecursive :: Recursive
  , optionsRawRegex :: RawRegex
  , optionsInputFilenames :: [InputFilename]
  } deriving (Eq, Read, Show)

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

rawRegexParser :: Parser RawRegex
rawRegexParser =
  let mods = metavar "PATTERN"
  in RawRegex <$> strArgument mods

inputFilenamesParser :: Parser [InputFilename]
inputFilenamesParser =
  let mods = metavar "FILE"
  in many $ InputFilename <$> strArgument mods

commonOptionsParser :: Parser Options
commonOptionsParser =
  Options
    <$> ignoreCaseParser
    <*> recursiveParser
    <*> rawRegexParser
    <*> inputFilenamesParser
