{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Highlight.Options
  ( ColorGrepFilenames(..)
  , colorGrepFilenamesParser
  , HasColorGrepFilenames(..)
  , Options(..)
  , defaultOptions
  , optionsParser
  , HasOptions(..)
  , module Highlight.Common.Options
  ) where

import Prelude ()
import Prelude.Compat

import Control.Lens (Lens', lens)
import Data.Monoid ((<>))
import Options.Applicative (Parser, flag, help, long, short)

import Highlight.Common.Options
       (CommonOptions, HasCommonOptions(commonOptionsLens),
        HasIgnoreCase(ignoreCaseLens), HasInputFilenames(inputFilenamesLens),
        HasRawRegex(rawRegexLens), HasRecursive(recursiveLens),
        IgnoreCase(DoNotIgnoreCase, IgnoreCase),
        InputFilename(InputFilename, unInputFilename), RawRegex(RawRegex),
        Recursive(Recursive), commonOptionsParser, defaultCommonOptions)

--------------------------
-- Color grep filenames --
--------------------------

data ColorGrepFilenames = ColorGrepFilenames | DoNotColorGrepFileNames
  deriving (Eq, Read, Show)

class HasColorGrepFilenames r where
  colorGrepFilenamesLens :: Lens' r ColorGrepFilenames

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
  optionsLens :: Lens' r Options

instance HasOptions Options where
  optionsLens :: Lens' Options Options
  optionsLens = id

instance HasColorGrepFilenames Options where
  colorGrepFilenamesLens :: Lens' Options ColorGrepFilenames
  colorGrepFilenamesLens =
    lens
      optionsColorGrepFilenames
      (\s a -> s {optionsColorGrepFilenames = a})

instance HasCommonOptions Options where
  commonOptionsLens :: Lens' Options CommonOptions
  commonOptionsLens =
    lens
      optionsCommonOptions
      (\s a -> s {optionsCommonOptions = a})

instance HasIgnoreCase Options
instance HasRecursive Options
instance HasRawRegex Options
instance HasInputFilenames Options

optionsParser :: Parser Options
optionsParser =
  Options
    <$> colorGrepFilenamesParser
    <*> commonOptionsParser

defaultOptions :: Options
defaultOptions = Options DoNotColorGrepFileNames defaultCommonOptions
