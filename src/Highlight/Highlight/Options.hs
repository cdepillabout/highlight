{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Highlight.Options
  ( ColorGrepFilenames(..)
  , colorGrepFilenamesParser
  , HasColorGrepFilenames(..)
  , Options(..)
  , optionsParser
  , HasOptions(..)
  , module Highlight.Common.Options
  ) where

import Prelude ()
import Prelude.Compat

import Data.Monoid ((<>))
import Options.Applicative (Parser, flag, help, long, short)

import Highlight.Common.Options
       (CommonOptions, HasCommonOptions(getCommonOptions),
        HasIgnoreCase(getIgnoreCase), HasInputFilenames(getInputFilenames),
        HasRawRegex(getRawRegex), HasRecursive(getRecursive),
        IgnoreCase(DoNotIgnoreCase, IgnoreCase),
        InputFilename(unInputFilename), RawRegex(RawRegex),
        Recursive(Recursive), commonOptionsParser)

--------------------------
-- Color grep filenames --
--------------------------

data ColorGrepFilenames = ColorGrepFilenames | DoNotColorGrepFileNames
  deriving (Eq, Read, Show)

class HasColorGrepFilenames r where
  getColorGrepFilenames :: r -> ColorGrepFilenames

instance HasColorGrepFilenames Options where
  getColorGrepFilenames :: Options -> ColorGrepFilenames
  getColorGrepFilenames = optionsColorGrepFilenames

colorGrepFilenamesParser :: Parser ColorGrepFilenames
colorGrepFilenamesParser =
  let mods =
        long "from-grep" <>
        short 'g' <>
        help "highlight output from grep"
  in flag DoNotColorGrepFileNames ColorGrepFilenames mods

-------------
-- Options --
-------------

data Options = Options
  { optionsColorGrepFilenames :: ColorGrepFilenames
  , optionsCommonOptions :: CommonOptions
  } deriving (Eq, Read, Show)

class HasOptions r where
  getOptions :: r -> Options

instance HasOptions Options where
  getOptions :: Options -> Options
  getOptions = id

instance HasIgnoreCase Options where
  getIgnoreCase :: Options -> IgnoreCase
  getIgnoreCase = getIgnoreCase . getCommonOptions

instance HasRecursive Options where
  getRecursive :: Options -> Recursive
  getRecursive = getRecursive . getCommonOptions

instance HasRawRegex Options where
  getRawRegex :: Options -> RawRegex
  getRawRegex = getRawRegex . getCommonOptions

instance HasInputFilenames Options where
  getInputFilenames :: Options -> [InputFilename]
  getInputFilenames = getInputFilenames . getCommonOptions

instance HasCommonOptions Options where
  getCommonOptions :: Options -> CommonOptions
  getCommonOptions = optionsCommonOptions

optionsParser :: Parser Options
optionsParser =
  Options
    <$> colorGrepFilenamesParser
    <*> commonOptionsParser
