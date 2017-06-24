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

import Control.Exception (IOException)
import Data.ByteString (ByteString)
import Pipes (Pipe, Producer, (>->), await, each, for, yield)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad
       (CommonHighlightM,
        FilenameHandlingFromFiles(NoFilename, PrintFilename),
        FileOrigin(FileFoundRecursively, FileSpecifiedByUser),
        FileProducer, InputData(InputDataFile, InputDataStdin),
        Output(OutputStderr, OutputStdout), compileHighlightRegexWithErr,
        computeFilenameHandlingFromFiles, createInputData,
        getFilePathFromFileOrigin, getIgnoreCaseM, getInputFilenamesM,
        getRawRegexM, getRecursiveM, outputConsumer, produerForSingleFile,
        runCommonHighlightM, throwRegexCompileErr)
import Highlight.Common.Options (CommonOptions)
import Highlight.Common.Pipes (numberedProducer)
import Highlight.Common.Util (convertStringToRawByteString)

--------------------
-- The Hrep Monad --
--------------------

type HrepM = CommonHighlightM CommonOptions () HighlightErr

runHrepM :: CommonOptions -> HrepM a -> IO (Either HighlightErr a)
runHrepM opts = runCommonHighlightM opts ()

-----------
-- Pipes --
-----------

handleInputData
  :: (ByteString -> [ByteString])
  -> ( FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> [ByteString]
     )
  -> (ByteString -> IOException -> Maybe IOException -> [ByteString])
  -> InputData HrepM ()
  -> Producer Output HrepM ()
handleInputData stdinFunc _ _ (InputDataStdin producer) =
  handleInputDataStdin stdinFunc producer
handleInputData _ handleNonError handleError (InputDataFile filenameHandling fileProducer) =
  handleInputDataFile handleNonError handleError filenameHandling fileProducer

handleInputDataStdin
  :: (ByteString -> [ByteString])
  -> Producer ByteString HrepM ()
  -> Producer Output HrepM ()
handleInputDataStdin f producer =
  producer >-> addNewline f
  where
    addNewline
      :: forall m. Monad m
      => (ByteString -> [ByteString])
      -> Pipe ByteString Output m ()
    addNewline func = go
      where
        go :: Pipe ByteString Output m ()
        go = do
          inputLine <- await
          let modifiedLine = fmap OutputStdout $ func inputLine
          case modifiedLine of
            [] -> go
            (_:_) -> do
              each modifiedLine
              yield $ OutputStdout "\n"
              go

handleInputDataFile
  :: ( FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> [ByteString]
     )
  -> (ByteString -> IOException -> Maybe IOException -> [ByteString])
  -> FilenameHandlingFromFiles
  -> FileProducer HrepM ()
  -> Producer Output HrepM ()
handleInputDataFile handleNonError handleError filenameHandling fileProducer =
  for (numberedProducer fileProducer) g
  where
    g
      :: ( Int
         , FileOrigin
         , Either
            (IOException, Maybe IOException)
            (Producer ByteString HrepM ())
         )
      -> Producer Output HrepM ()
    g (_, fileOrigin, Left (ioerr, maybeioerr)) = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      byteStringFilePath <- convertStringToRawByteString filePath
      let outputLines =
            fmap OutputStderr $ handleError byteStringFilePath ioerr maybeioerr
      each outputLines
      yield $ OutputStderr "\n"
    g (fileNumber, fileOrigin, Right producer) = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      byteStringFilePath <- convertStringToRawByteString filePath
      producer >-> bababa byteStringFilePath
      where
        bababa :: ByteString -> Pipe ByteString Output HrepM ()
        bababa filePath = go
          where
            go :: Pipe ByteString Output HrepM ()
            go = do
              inputLine <- await
              let outputLines =
                    fmap OutputStdout $
                      handleNonError
                        filenameHandling
                        filePath
                        fileNumber
                        inputLine
              case outputLines of
                [] -> go
                (_:_) -> do
                  each outputLines
                  yield $ OutputStdout "\n"
                  go
