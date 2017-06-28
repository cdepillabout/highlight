{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Common.Options where

import Prelude ()
import Prelude.Compat

import Control.Applicative (many)
import Control.Lens (Lens', lens)
import Data.Monoid ((<>))
import Data.String (IsString)
import Options.Applicative
       (Parser, flag, help, long, metavar, short, strArgument)

-----------------
-- Ignore case --
-----------------

-- | Whether or not the case of a regular expression should be ignored.
-- Similar to @grep@'s @--ignore-case@ option.
data IgnoreCase = IgnoreCase | DoNotIgnoreCase
  deriving (Eq, Read, Show)

class HasIgnoreCase r where
  ignoreCaseLens :: Lens' r IgnoreCase
  default ignoreCaseLens :: HasCommonOptions r => Lens' r IgnoreCase
  ignoreCaseLens = commonOptionsLens . ignoreCaseLens

ignoreCaseParser :: Parser IgnoreCase
ignoreCaseParser =
  flag
    DoNotIgnoreCase
    IgnoreCase
    (long "ignore-case" <> short 'i' <> help "ignore case distinctions")

---------------
-- Recursive --
---------------

-- | Whether or not files should be searched recursively.  Similar to @grep@'s
-- @--recursive@ option.
data Recursive = Recursive | NotRecursive
  deriving (Eq, Read, Show)

class HasRecursive r where
  recursiveLens :: Lens' r Recursive
  default recursiveLens :: HasCommonOptions r => Lens' r Recursive
  recursiveLens = commonOptionsLens . recursiveLens

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

-- | The raw, pre-compiled regular expression passed in on the command line by
-- the user.
newtype RawRegex = RawRegex
  { unRawRegex :: String
  } deriving (Eq, IsString, Read, Show)

class HasRawRegex r where
  rawRegexLens :: Lens' r RawRegex
  default rawRegexLens :: HasCommonOptions r => Lens' r RawRegex
  rawRegexLens = commonOptionsLens . rawRegexLens

rawRegexParser :: Parser RawRegex
rawRegexParser =
  let mods = metavar "PATTERN"
  in RawRegex <$> strArgument mods

--------------------
-- input filename --
--------------------

-- | An input file passed in on the command line by the user.
newtype InputFilename = InputFilename
  { unInputFilename :: FilePath
  } deriving (Eq, IsString, Read, Show)

class HasInputFilenames r where
  inputFilenamesLens :: Lens' r [InputFilename]
  default inputFilenamesLens :: HasCommonOptions r => Lens' r [InputFilename]
  inputFilenamesLens = commonOptionsLens . inputFilenamesLens

inputFilenamesParser :: Parser [InputFilename]
inputFilenamesParser =
  let mods = metavar "FILE"
  in many $ InputFilename <$> strArgument mods

--------------------
-- common options --
--------------------

-- | A set of options that are common to both the @highlight@ and @hrep@
-- applications.
data CommonOptions = CommonOptions
  { commonOptionsIgnoreCase :: IgnoreCase
  , commonOptionsRecursive :: Recursive
  , commonOptionsRawRegex :: RawRegex
  , commonOptionsInputFilenames :: [InputFilename]
  } deriving (Eq, Read, Show)

class HasCommonOptions r where
  commonOptionsLens :: Lens' r CommonOptions

instance HasCommonOptions CommonOptions where
  commonOptionsLens :: Lens' CommonOptions CommonOptions
  commonOptionsLens = id

instance HasIgnoreCase CommonOptions where
  ignoreCaseLens :: Lens' CommonOptions IgnoreCase
  ignoreCaseLens =
    lens
      commonOptionsIgnoreCase
      (\s a -> s {commonOptionsIgnoreCase = a})

instance HasRecursive CommonOptions where
  recursiveLens :: Lens' CommonOptions Recursive
  recursiveLens =
    lens
      commonOptionsRecursive
      (\s a -> s {commonOptionsRecursive = a})

instance HasRawRegex CommonOptions where
  rawRegexLens :: Lens' CommonOptions RawRegex
  rawRegexLens =
    lens
      commonOptionsRawRegex
      (\s a -> s {commonOptionsRawRegex = a})

instance HasInputFilenames CommonOptions where
  inputFilenamesLens :: Lens' CommonOptions [InputFilename]
  inputFilenamesLens =
    lens
      commonOptionsInputFilenames
      (\s a -> s {commonOptionsInputFilenames = a})

commonOptionsParser :: Parser CommonOptions
commonOptionsParser =
  CommonOptions
    <$> ignoreCaseParser
    <*> recursiveParser
    <*> rawRegexParser
    <*> inputFilenamesParser

-- | A default set of 'CommonOptions'.  Defined as the following:
--
-- >>> :{
-- let opts =
--       CommonOptions
--         { commonOptionsIgnoreCase = DoNotIgnoreCase
--         , commonOptionsRecursive = NotRecursive
--         , commonOptionsRawRegex = RawRegex { unRawRegex = "" }
--         , commonOptionsInputFilenames = []
--         }
-- :}
--
-- >>> opts == defaultCommonOptions
-- True
defaultCommonOptions :: CommonOptions
defaultCommonOptions =
  CommonOptions DoNotIgnoreCase NotRecursive (RawRegex "") []
