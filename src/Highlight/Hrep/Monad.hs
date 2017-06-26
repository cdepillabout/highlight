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
       -- (CommonHighlightM,
       --  FilenameHandlingFromFiles(NoFilename, PrintFilename),
       --  FileOrigin(FileFoundRecursively, FileSpecifiedByUser),
       --  FileProducer, InputData(InputDataFile, InputDataStdin),
       --  Output(OutputStderr, OutputStdout), compileHighlightRegexWithErr,
       --  computeFilenameHandlingFromFiles, createInputData,
       --  getFilePathFromFileOrigin, getIgnoreCaseM, getInputFilenamesM,
       --  getRawRegexM, getRecursiveM, outputConsumer, produerForSingleFile,
       --  runCommonHighlightM, throwRegexCompileErr)
import Highlight.Common.Options (CommonOptions)

--------------------
-- The Hrep Monad --
--------------------

type HrepM = CommonHighlightM CommonOptions () HighlightErr

runHrepM :: CommonOptions -> HrepM a -> IO (Either HighlightErr a)
runHrepM opts = runCommonHighlightM opts ()
