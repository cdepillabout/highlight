{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask, reader, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Pipes
       (Effect, Pipe, Producer, (>->), await, each, for, next, runEffect,
        yield)
import qualified Pipes.ByteString
import Pipes.Prelude (toListM)
import Pipes.Safe (runSafeT)
import System.IO (stdin)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad
       (CommonHighlightM,
        FilenameHandlingFromFiles(NoFilename, PrintFilename),
        FileOrigin(FileFoundRecursively, FileSpecifiedByUser),
        FileProducer, Output(OutputStderr, OutputStdout),
        computeFilenameHandlingFromFiles, getFilePathFromFileOrigin,
        getIgnoreCaseM, getInputFilenamesM, getRawRegexM, getRecursiveM,
        produerForSingleFile, runCommonHighlightM, throwRegexCompileErr)
import Highlight.Common.Options
       (CommonOptions, IgnoreCase, InputFilename(unInputFilename),
        RawRegex, Recursive(Recursive))
import Highlight.Common.Pipes
       (childOf, fromHandleLines, numberedProducer, stderrConsumer)
import Highlight.Common.Util
       (combineApplicatives, convertStringToRawByteString,
        openFilePathForReading)

--------------------
-- The Hrep Monad --
--------------------

type HrepM = CommonHighlightM CommonOptions () HighlightErr

runHrepM :: CommonOptions -> HrepM a -> IO (Either HighlightErr a)
runHrepM opts = runCommonHighlightM opts ()

-----------
-- Pipes --
-----------

createInputData
  :: Producer ByteString HrepM ()
  -> HrepM (InputData HrepM ())
createInputData stdinProducer = do
  inputFilenames <-
    fmap (FileSpecifiedByUser . unInputFilename) <$> getInputFilenamesM
  recursive <- getRecursiveM
  case inputFilenames of
    [] -> do
      return $ InputDataStdin stdinProducer
    (file1:files) -> do
      let lalas =
            fmap
              (produerForSingleFile recursive)
              (file1 :| files)
      let fileProducer = foldl1 combineApplicatives lalas
      (filenameHandling, newHighlightFileProducer) <-
        computeFilenameHandlingFromFiles fileProducer
      return $ InputDataFile filenameHandling newHighlightFileProducer

data InputData m a
  = InputDataStdin (Producer ByteString m a)
  | InputDataFile
      FilenameHandlingFromFiles
      (FileProducer m a)

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
handleInputData _ handleNonError handleError (InputDataFile filenameHandling fileProducer) = do
  handleInputDataFile handleNonError handleError filenameHandling fileProducer

handleInputDataStdin
  :: (ByteString -> [ByteString])
  -> Producer ByteString HrepM ()
  -> Producer Output HrepM ()
handleInputDataStdin f producer = do
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
handleInputDataFile handleNonError handleError filenameHandling fileProducer = do
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
