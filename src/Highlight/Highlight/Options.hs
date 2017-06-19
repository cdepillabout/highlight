{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Highlight.Options where

import Prelude ()
import Prelude.Compat

import Control.Applicative (many)
import Data.Monoid ((<>))
import Options.Applicative
       (Parser, flag, help, long, metavar, short, strArgument)

data ColorGrepFilenames = ColorGrepFilenames | DoNotColorGrepFileNames
  deriving (Eq, Read, Show)

data Options = Options
  { optionsIgnoreCase :: IgnoreCase
  , optionsRecursive :: Recursive
  , optionsColorGrepFilenames :: ColorGrepFilenames
  , optionsRawRegex :: RawRegex
  , optionsInputFilenames :: [InputFilename]
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
    <$> ignoreCaseParser
    <*> recursiveParser
    <*> colorGrepFilenamesParser
    <*> rawRegexParser
    <*> inputFilenamesParser
