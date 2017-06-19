{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Highlight.Options
  ( ColorGrepFilenames(..)
  , colorGrepFilenamesParser
  , Options(..)
  , optionsParser
  , module Highlight.Common.Options
  ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (many)
import Data.Monoid ((<>))
import Options.Applicative
       (Parser, flag, help, long, metavar, short, strArgument)

import Highlight.Common.Options
       (CommonOptions, IgnoreCase, InputFilename(unInputFilename), RawRegex, Recursive(Recursive),
        commonOptionsParser)

data ColorGrepFilenames = ColorGrepFilenames | DoNotColorGrepFileNames
  deriving (Eq, Read, Show)

data Options = Options
  { optionsColorGrepFilenames :: ColorGrepFilenames
  , optionsCommonOptions :: CommonOptions
  } deriving (Eq, Read, Show)

colorGrepFilenamesParser :: Parser ColorGrepFilenames
colorGrepFilenamesParser =
  let mods =
        long "from-grep" <>
        short 'g' <>
        help "highlight output from grep"
  in flag DoNotColorGrepFileNames ColorGrepFilenames mods

optionsParser :: Parser Options
optionsParser =
  Options
    <$> colorGrepFilenamesParser
    <*> commonOptionsParser
