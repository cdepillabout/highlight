{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Exception (IOException, try)
import Control.Lens (view)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, reader, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid ((<>))
import Data.String (IsString)
import Pipes
       (Consumer, Effect, Pipe, Producer, (>->), await, each, for, next, runEffect,
        yield)
import Pipes.ByteString (stdout)
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
        outputConsumer, produerForSingleFile, runCommonHighlightM,
        throwRegexCompileErr)
import Highlight.Common.Pipes
       (childOf, fromHandleLines, numberedProducer, stderrConsumer)
import Highlight.Common.Util
       (combineApplicatives, convertStringToRawByteString,
        openFilePathForReading)
import Highlight.Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        HasColorGrepFilenames(colorGrepFilenamesLens), IgnoreCase,
        InputFilename(unInputFilename), Options(..), RawRegex,
        Recursive(Recursive))

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

-----------
-- Pipes --
-----------

createInputData
  :: Producer ByteString HighlightM ()
  -> HighlightM (InputData HighlightM ())
createInputData stdinProducer = do
  inputFilenames <-
    fmap (FileSpecifiedByUser . unInputFilename) <$> getInputFilenamesM
  recursive <- getRecursiveM
  colorGrepFilenames <- getColorGrepFilenamesM
  case inputFilenames of
    [] -> do
      let filenameHandling = computeFilenameHandlingFromStdin colorGrepFilenames
      return $ InputDataStdin filenameHandling stdinProducer
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
  = InputDataStdin
      !FilenameHandlingFromStdin
      !(Producer ByteString m a)
  | InputDataFile
      !FilenameHandlingFromFiles
      !(FileProducer m a)

handleInputData
  :: (FilenameHandlingFromStdin -> ByteString -> HighlightM [ByteString])
  -> ( FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> [ByteString]
     )
  -> (ByteString -> IOException -> Maybe IOException -> [ByteString])
  -> InputData HighlightM ()
  -> Producer Output HighlightM ()
handleInputData stdinFunc _ _ (InputDataStdin filenameHandling producer) =
  handleInputDataStdin stdinFunc filenameHandling producer
handleInputData _ handleNonError handleError (InputDataFile filenameHandling fileProducer) = do
  handleInputDataFile handleNonError handleError filenameHandling fileProducer

handleInputDataStdin
  :: ( FilenameHandlingFromStdin
        -> ByteString
        -> HighlightM [ByteString]
     )
  -> FilenameHandlingFromStdin
  -> Producer ByteString HighlightM ()
  -> Producer Output HighlightM ()
handleInputDataStdin f filenameHandling producer =
  producer >-> addNewline (f filenameHandling)
  where
    addNewline
      :: forall m. Monad m
      => (ByteString -> m [ByteString])
      -> Pipe ByteString Output m ()
    addNewline func = go
      where
        go :: Pipe ByteString Output m ()
        go = do
          inputLine <- await
          modifiedLine <- lift $ func inputLine >>= return . fmap OutputStdout
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
  -> FileProducer HighlightM ()
  -> Producer Output HighlightM ()
handleInputDataFile handleNonError handleError filenameHandling fileProducer =
  for (numberedProducer fileProducer) g
  where
    g
      :: ( Int
         , FileOrigin
         , Either
            (IOException, Maybe IOException)
            (Producer ByteString HighlightM ())
         )
      -> Producer Output HighlightM ()
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
        bababa :: ByteString -> Pipe ByteString Output HighlightM ()
        bababa filePath = go
          where
            go :: Pipe ByteString Output HighlightM ()
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
