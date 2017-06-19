{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Common.Options where

import Prelude ()
import Prelude.Compat

import Control.Applicative (many)
import Data.Monoid ((<>))
import Options.Applicative
       (Parser, flag, help, long, metavar, short, strArgument)

-----------------
-- Ignore case --
-----------------

data IgnoreCase = IgnoreCase | DoNotIgnoreCase
  deriving (Eq, Read, Show)

class HasIgnoreCase r where
  getIgnoreCase :: r -> IgnoreCase

instance HasIgnoreCase CommonOptions where
  getIgnoreCase :: CommonOptions -> IgnoreCase
  getIgnoreCase = commonOptionsIgnoreCase

ignoreCaseParser :: Parser IgnoreCase
ignoreCaseParser =
  flag
    DoNotIgnoreCase
    IgnoreCase
    (long "ignore-case" <> short 'i' <> help "ignore case distinctions")

---------------
-- Recursive --
---------------

data Recursive = Recursive | NotRecursive
  deriving (Eq, Read, Show)

class HasRecursive r where
  getRecursive :: r -> Recursive

instance HasRecursive CommonOptions where
  getRecursive :: CommonOptions -> Recursive
  getRecursive = commonOptionsRecursive

recursiveParser :: Parser Recursive
recursiveParser =
  let mods =
        long "recursive" <>
        short 'r' <>
        help "recursive operate on files under specified directory"
  in flag NotRecursive Recursive mods

---------------
-- Raw regex --
---------------

newtype RawRegex = RawRegex
  { unRawRegex :: String
  } deriving (Eq, Read, Show)

class HasRawRegex r where
  getRawRegex :: r -> RawRegex

instance HasRawRegex CommonOptions where
  getRawRegex :: CommonOptions -> RawRegex
  getRawRegex = commonOptionsRawRegex

rawRegexParser :: Parser RawRegex
rawRegexParser =
  let mods = metavar "PATTERN"
  in RawRegex <$> strArgument mods

--------------------
-- input filename --
--------------------

newtype InputFilename = InputFilename
  { unInputFilename :: FilePath
  } deriving (Eq, Read, Show)

class HasInputFilenames r where
  getInputFilenames :: r -> [InputFilename]

instance HasInputFilenames CommonOptions where
  getInputFilenames :: CommonOptions -> [InputFilename]
  getInputFilenames = commonOptionsInputFilenames

inputFilenamesParser :: Parser [InputFilename]
inputFilenamesParser =
  let mods = metavar "FILE"
  in many $ InputFilename <$> strArgument mods

--------------------
-- common options --
--------------------

data CommonOptions = CommonOptions
  { commonOptionsIgnoreCase :: IgnoreCase
  , commonOptionsRecursive :: Recursive
  , commonOptionsRawRegex :: RawRegex
  , commonOptionsInputFilenames :: [InputFilename]
  } deriving (Eq, Read, Show)

class HasCommonOptions r where
  getCommonOptions :: r -> CommonOptions

instance HasCommonOptions CommonOptions where
  getCommonOptions :: CommonOptions -> CommonOptions
  getCommonOptions = id

commonOptionsParser :: Parser CommonOptions
commonOptionsParser =
  CommonOptions
    <$> ignoreCaseParser
    <*> recursiveParser
    <*> rawRegexParser
    <*> inputFilenamesParser
