{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Highlight.Monad
  ( module Highlight.Highlight.Monad
  , module Highlight.Common.Monad
  ) where

import Prelude ()
import Prelude.Compat

import Control.Lens (view)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, get, put)
import Data.ByteString (ByteString)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad
       -- (CommonHighlightM,
       --  FilenameHandlingFromFiles(NoFilename, PrintFilename),
       --  FileOrigin(FileFoundRecursively, FileSpecifiedByUser),
       --  FileProducer, InputData(InputDataFile, InputDataStdin),
       --  Output(OutputStderr, OutputStdout), compileHighlightRegexWithErr,
       --  computeFilenameHandlingFromFiles, createInputData,
       --  getFilePathFromFileOrigin, getIgnoreCaseM, getInputFilenamesM,
       --  getRawRegexM, getRecursiveM, outputConsumer, produerForSingleFile,
       --  runCommonHighlightM, throwRegexCompileErr)
import Highlight.Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        HasColorGrepFilenames(colorGrepFilenamesLens), Options(..))

data FromGrepFilenameState = FromGrepFilenameState
  { fromGrepFilenameStatePrevFileNum :: {-# UNPACK #-} !Int
  , fromGrepFilenameStatePrevFilename :: !(Maybe ByteString)
  }

initFromGrepFilenameState :: FromGrepFilenameState
initFromGrepFilenameState =
  FromGrepFilenameState
  { fromGrepFilenameStatePrevFileNum = (-1)
  , fromGrepFilenameStatePrevFilename = Nothing
  }

updateFilename :: MonadState FromGrepFilenameState m => ByteString -> m Int
updateFilename nextFilename = do
  FromGrepFilenameState prevFileNum prevFilename <- get
  let justNextFilename = Just nextFilename
  if justNextFilename == prevFilename
    then return prevFileNum
    else do
      let nextFileNum = prevFileNum + 1
      put $ FromGrepFilenameState nextFileNum justNextFilename
      return nextFileNum

-------------------------
-- The Highlight Monad --
-------------------------

type HighlightM = CommonHighlightM Options FromGrepFilenameState HighlightErr

runHighlightM :: Options -> HighlightM a -> IO (Either HighlightErr a)
runHighlightM opts = runCommonHighlightM opts initFromGrepFilenameState

----------------------------------
-- Get value of certain options --
----------------------------------

getColorGrepFilenamesM
  :: (HasColorGrepFilenames r, MonadReader r m) => m ColorGrepFilenames
getColorGrepFilenamesM = view colorGrepFilenamesLens

-----------------------
-- Filename Handling --
-----------------------

data FilenameHandlingFromStdin
  = FromStdinNoFilename
  | FromStdinParseFilenameFromGrep
  deriving (Eq, Read, Show)

computeFilenameHandlingFromStdin
  :: ColorGrepFilenames -> FilenameHandlingFromStdin
computeFilenameHandlingFromStdin ColorGrepFilenames = FromStdinParseFilenameFromGrep
computeFilenameHandlingFromStdin DoNotColorGrepFileNames = FromStdinNoFilename

filenameHandlingFromStdinM
  :: (HasColorGrepFilenames r, MonadReader r m)
  => m FilenameHandlingFromStdin
filenameHandlingFromStdinM =
  fmap computeFilenameHandlingFromStdin getColorGrepFilenamesM
