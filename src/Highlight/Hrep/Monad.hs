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
        FileProducer, computeFilenameHandlingFromFiles,
        getFilePathFromFileOrigin, getIgnoreCaseM, getInputFilenamesM,
        getRecursiveM, produerForSingleFile, runCommonHighlightM)
import Highlight.Common.Options
       (CommonOptions, IgnoreCase, InputFilename(unInputFilename),
        RawRegex, Recursive(Recursive))
import Highlight.Common.Pipes
       (childOf, fromHandleLines, numberedProducer, stderrConsumer)
import Highlight.Common.Util
       (combineApplicatives, convertStringToRawByteString,
        openFilePathForReading)

-------------------------
-- The Highlight Monad --
-------------------------

type HighlightM = CommonHighlightM CommonOptions () HighlightErr

runHighlightM :: CommonOptions -> HighlightM a -> IO (Either HighlightErr a)
runHighlightM opts = runCommonHighlightM opts ()

-----------
-- Pipes --
-----------

createInputData :: HighlightM (InputData HighlightM ())
createInputData = do
  inputFilenames <-
    fmap (FileSpecifiedByUser . unInputFilename) <$> getInputFilenamesM
  recursive <- getRecursiveM
  case inputFilenames of
    [] -> do
      return . InputDataStdin $ fromHandleLines stdin
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
  :: (ByteString -> HighlightM (NonEmpty ByteString))
  -> ( FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> NonEmpty ByteString
     )
  -> ( ByteString
        -> IOException
        -> Maybe IOException
        -> NonEmpty ByteString
      )
  -> InputData HighlightM ()
  -> HighlightM ()
handleInputData stdinFunc _ _ (InputDataStdin producer) =
  handleInputDataStdin stdinFunc producer
handleInputData _ handleNonError handleError (InputDataFile filenameHandling fileProducer) = do
  handleInputDataFile handleNonError handleError filenameHandling fileProducer

handleInputDataStdin
  :: (ByteString -> HighlightM (NonEmpty ByteString))
  -> Producer ByteString HighlightM ()
  -> HighlightM ()
handleInputDataStdin f producer = do
  runEffect $ producer >-> addNewline f >-> Pipes.ByteString.stdout
  where
    addNewline
      :: forall m. Monad m
      => (ByteString -> m (NonEmpty ByteString))
      -> Pipe ByteString ByteString m ()
    addNewline func = go
      where
        go :: Pipe ByteString ByteString m ()
        go = do
          inputLine <- await
          modifiedLine <- lift $ func inputLine
          each modifiedLine
          yield "\n"
          go

handleInputDataFile
  :: ( FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> NonEmpty ByteString
     )
  -> (ByteString -> IOException -> Maybe IOException -> NonEmpty ByteString)
  -> FilenameHandlingFromFiles
  -> FileProducer HighlightM ()
  -> HighlightM ()
handleInputDataFile handleNonError handleError filenameHandling fileProducer = do
  runEffect $ for (numberedProducer fileProducer) g
  where
    g
      :: ( Int
         , FileOrigin
         , Either
            (IOException, Maybe IOException)
            (Producer ByteString HighlightM ())
         )
      -> Effect HighlightM ()
    g (_, fileOrigin, Left (ioerr, maybeioerr)) = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      byteStringFilePath <- convertStringToRawByteString filePath
      let outputLines = handleError byteStringFilePath ioerr maybeioerr
      (each outputLines *> yield "\n") >-> stderrConsumer
    g (fileNumber, fileOrigin, Right producer) = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      byteStringFilePath <- convertStringToRawByteString filePath
      producer >-> bababa byteStringFilePath >-> Pipes.ByteString.stdout
      where
        bababa :: ByteString -> Pipe ByteString ByteString HighlightM ()
        bababa filePath = do
          inputLine <- await
          let outputLines =
                handleNonError filenameHandling filePath fileNumber inputLine
          each outputLines
          yield "\n"
          bababa filePath
