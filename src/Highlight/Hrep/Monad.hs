{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Hrep.Monad
  ( module Highlight.Hrep.Monad
  , module Highlight.Common.Monad
  ) where

import Prelude ()
import Prelude.Compat

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad
       (CommonHighlightM,
        FilenameHandlingFromFiles(NoFilename, PrintFilename), InputData,
        Output, compileHighlightRegexWithErr, createInputData,
        getInputFilenamesM, getRecursiveM, handleInputData,
        runCommonHighlightM, runOutputProducer)
import Highlight.Common.Options (CommonOptions)

--------------------
-- The Hrep Monad --
--------------------

-- | 'HrepM' is just 'CommonHighlightM' specialized for @hrep@.
type HrepM = CommonHighlightM CommonOptions () HighlightErr

runHrepM :: CommonOptions -> HrepM a -> IO (Either HighlightErr a)
runHrepM opts = runCommonHighlightM opts ()
