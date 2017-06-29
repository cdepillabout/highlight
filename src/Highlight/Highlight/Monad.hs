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
import Control.Monad.State (MonadState, get)
import Data.ByteString (ByteString)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad
       (CommonHighlightM,
        FilenameHandlingFromFiles(NoFilename, PrintFilename), InputData,
        Output, compileHighlightRegexWithErr, createInputData,
        getInputFilenamesM, getRecursiveM, handleInputData,
        outputConsumer, runCommonHighlightM)
import Highlight.Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        HasColorGrepFilenames(colorGrepFilenamesLens), Options(..))
import Highlight.Util (modify')

-- | The internal state that is used to figure out how to color filenames from
-- @grep@.
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

-- | Call 'updateFilename' and return the new file number after doing the
-- update.
updateFilenameM :: MonadState FromGrepFilenameState m => ByteString -> m Int
updateFilenameM nextFilename = do
  modify' $ updateFilename nextFilename
  FromGrepFilenameState newFileNum _ <- get
  return newFileNum

-- | Update the file number in 'FromGrepFilenameState' if the 'ByteString'
-- filename passed in is different from that in 'FromGrepFilenameState'.
updateFilename :: ByteString -> FromGrepFilenameState -> FromGrepFilenameState
updateFilename nextFilename (FromGrepFilenameState prevFileNum prevFilename)
  | Just nextFilename == prevFilename =
    FromGrepFilenameState prevFileNum prevFilename
  | otherwise =
    FromGrepFilenameState (prevFileNum + 1) (Just nextFilename)

-------------------------
-- The Highlight Monad --
-------------------------

-- | 'HighlightM' is just 'CommonHighlightM' specialized for @highlight@.
type HighlightM = CommonHighlightM Options FromGrepFilenameState HighlightErr

runHighlightM :: Options -> HighlightM a -> IO (Either HighlightErr a)
runHighlightM opts = runCommonHighlightM opts initFromGrepFilenameState

----------------------------------
-- Get value of certain options --
----------------------------------

-- | Get the value of the 'ColorGrepFilenames' option.
getColorGrepFilenamesM
  :: (HasColorGrepFilenames r, MonadReader r m) => m ColorGrepFilenames
getColorGrepFilenamesM = view colorGrepFilenamesLens
