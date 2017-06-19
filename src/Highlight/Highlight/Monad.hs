{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Highlight.Monad
  ( module Highlight.Highlight.Monad
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
        getRawRegexM, getRecursiveM, produerForSingleFile,
        runCommonHighlightM, throwRegexCompileErr)
import Highlight.Common.Pipes
       (childOf, fromHandleLines, numberedProducer, stderrConsumer)
import Highlight.Common.Util
       (combineApplicatives, convertStringToRawByteString,
        openFilePathForReading)
import Highlight.Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        HasColorGrepFilenames(getColorGrepFilenames), IgnoreCase,
        InputFilename(unInputFilename), Options(..), RawRegex,
        Recursive(Recursive))

data FromGrepFilenameState = FromGrepFilenameState
  { fromGrepFilenameStatePrevFileNum :: Int
  , fromGrepFilenameStatePrevFilename :: (Maybe ByteString)
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
  :: (HasColorGrepFilenames r, MonadReader r m)
  => m ColorGrepFilenames
getColorGrepFilenamesM = reader getColorGrepFilenames

-----------
-- Pipes --
-----------

createInputData :: HighlightM (InputData HighlightM ())
createInputData = do
  inputFilenames <-
    fmap (FileSpecifiedByUser . unInputFilename) <$> getInputFilenamesM
  recursive <- getRecursiveM
  colorGrepFilenames <- getColorGrepFilenamesM
  case inputFilenames of
    [] -> do
      let filenameHandling = computeFilenameHandlingFromStdin colorGrepFilenames
      return . InputDataStdin filenameHandling $ fromHandleLines stdin
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
      FilenameHandlingFromStdin
      (Producer ByteString m a)
  | InputDataFile
      FilenameHandlingFromFiles
      (FileProducer m a)

handleInputData
  :: ( FilenameHandlingFromStdin
        -> ByteString
        -> HighlightM (NonEmpty ByteString)
     )
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
handleInputData stdinFunc _ _ (InputDataStdin filenameHandling producer) =
  handleInputDataStdin stdinFunc filenameHandling producer
handleInputData _ handleNonError handleError (InputDataFile filenameHandling fileProducer) = do
  handleInputDataFile handleNonError handleError filenameHandling fileProducer

handleInputDataStdin
  :: ( FilenameHandlingFromStdin
        -> ByteString
        -> HighlightM (NonEmpty ByteString)
     )
  -> FilenameHandlingFromStdin
  -> Producer ByteString HighlightM ()
  -> HighlightM ()
handleInputDataStdin f filenameHandling producer = do
  runEffect $
    producer >-> addNewline (f filenameHandling) >-> Pipes.ByteString.stdout
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
