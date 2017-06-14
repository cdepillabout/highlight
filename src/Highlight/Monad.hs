{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Monad where

import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask, reader, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.DirStream (childOf)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Pipes
       (Effect, Pipe, Producer, (>->), await, each, enumerate, for, next,
        runEffect, yield)
import qualified Pipes.ByteString
import Pipes.Prelude (toListM)
import Pipes.Safe (runSafeT)
import System.IO (stdin)

import Highlight.Error (HighlightErr(..))
import Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        IgnoreCase, InputFilename(unInputFilename), Options(..), RawRegex,
        Recursive(Recursive))
import Highlight.Pipes (fromHandleLines, numberedProducer, stderrConsumer)
import Highlight.Util
       (combineApplicatives, convertStringToRawByteString,
        openFilePathForReading)

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
    then pure prevFileNum
    else do
      let nextFileNum = prevFileNum + 1
      put $ FromGrepFilenameState nextFileNum justNextFilename
      pure nextFileNum

-------------------------
-- The Highlight Monad --
-------------------------

newtype HighlightM a = HighlightM
  { unHighlightM
      :: ReaderT
          Options
          (StateT FromGrepFilenameState (ExceptT HighlightErr IO))
          a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError HighlightErr
             , MonadIO
             , MonadReader Options
             , MonadState FromGrepFilenameState
             )

runHighlightM :: Options -> HighlightM a -> IO (Either HighlightErr a)
runHighlightM opts =
  runExceptT .
    flip evalStateT initFromGrepFilenameState .
    flip runReaderT opts .
    unHighlightM

----------------------------------
-- Get value of certain options --
----------------------------------

getOptions :: HighlightM Options
getOptions = ask

getIgnoreCase :: HighlightM IgnoreCase
getIgnoreCase  = reader optionsIgnoreCase

getRecursive :: HighlightM Recursive
getRecursive = reader optionsRecursive

getColorGrepFilenames :: HighlightM ColorGrepFilenames
getColorGrepFilenames = reader optionsColorGrepFilenames

getRawRegex :: HighlightM RawRegex
getRawRegex = reader optionsRawRegex

getInputFilenames :: HighlightM [InputFilename]
getInputFilenames = reader optionsInputFilenames

------------------
-- Throw Errors --
------------------

throwHighlightErr :: HighlightErr -> HighlightM a
throwHighlightErr = throwError

throwRegexCompileErr :: RawRegex -> HighlightM a
throwRegexCompileErr = throwHighlightErr . HighlightRegexCompileErr

-----------
-- Pipes --
-----------

-- | TODO: This needs to be changed to just produce a list of
-- WhereDidFileComeFroms.  The actual file opening and Producer creation needs
-- to be done later, one file at a time, probably in the handleInputData
-- function....
type Lala m a =
  Producer
    ( WhereDidFileComeFrom
    , Either
        (IOException, Maybe IOException)
        (Producer ByteString m a)
    )
    m
    a

data WhereDidFileComeFrom
  = FileSpecifiedByUser FilePath
  | FileFoundRecursively FilePath

getFilePathFromWhereDid :: WhereDidFileComeFrom -> FilePath
getFilePathFromWhereDid (FileSpecifiedByUser fp) = fp
getFilePathFromWhereDid (FileFoundRecursively fp) = fp

createInputData :: HighlightM (InputData HighlightM ())
createInputData = do
  inputFilenames <-
    fmap (FileSpecifiedByUser . unInputFilename) <$> getInputFilenames
  recursive <- getRecursive
  colorGrepFilenames <- getColorGrepFilenames
  case inputFilenames of
    [] -> do
      let filenameHandling = computeFilenameHandlingFromStdin colorGrepFilenames
      pure . InputDataStdin filenameHandling $ fromHandleLines stdin
    (file1:files) -> do
      let lalas =
            fmap
              (producerForSingleFilePossiblyRecursive recursive)
              (file1 :| files)
      let lala = foldl1 combineApplicatives lalas
      filenameHandling <- computeFilenameHandlingFromFiles lala
      pure $ InputDataFile filenameHandling lala

-- | TODO: This is somewhat complicated.
producerForSingleFilePossiblyRecursive
  :: forall m.
     MonadIO m
  => Recursive -> WhereDidFileComeFrom -> Lala m ()
producerForSingleFilePossiblyRecursive recursive = go
  where
    go :: WhereDidFileComeFrom -> Lala m ()
    go whereDid = do
      let filePath = getFilePathFromWhereDid whereDid
      eitherHandle <- openFilePathForReading filePath
      case eitherHandle of
        Right handle -> do
          let linesProducer = fromHandleLines handle
          yield (whereDid, Right linesProducer)
        Left fileIOErr ->
          if recursive == Recursive
            then do
              let listT = childOf $ decodeString filePath
                  producer =
                    enumerate listT
                  lIO = runSafeT $ toListM producer
              eitherFileList <- liftIO (try lIO)
              case eitherFileList of
                Left dirIOErr ->
                  yield (whereDid, Left (fileIOErr, Just dirIOErr))
                Right fileList -> do
                  let sortedFileList = sort fileList
                  let whereDids =
                        fmap
                          (FileFoundRecursively . encodeString)
                          sortedFileList
                  let lalas =
                        fmap
                          (producerForSingleFilePossiblyRecursive recursive)
                          whereDids
                  for (each lalas) id
            else
              yield (whereDid, Left (fileIOErr, Nothing))

data InputData m a
  = InputDataStdin
      FilenameHandlingFromStdin
      (Producer ByteString m a)
  | InputDataFile
      FilenameHandlingFromFiles
      (Lala m a)

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
handleInputData _ handleNonError handleError (InputDataFile filenameHandling lala) = do
  handleInputDataFile handleNonError handleError filenameHandling lala

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
  -> Lala HighlightM ()
  -> HighlightM ()
handleInputDataFile handleNonError handleError filenameHandling lala = do
  runEffect $ for (numberedProducer lala) g
  where
    g
      :: ( Int
         , WhereDidFileComeFrom
         , Either
            (IOException, Maybe IOException)
            (Producer ByteString HighlightM ())
         )
      -> Effect HighlightM ()
    g (_, whereDid, Left (ioerr, maybeioerr)) = do
      let filePath = getFilePathFromWhereDid whereDid
      byteStringFilePath <- convertStringToRawByteString filePath
      let outputLines = handleError byteStringFilePath ioerr maybeioerr
      each outputLines >-> stderrConsumer
      -- let producer =
      --       case maybeioerr of
      --         Just ioerr -> do
      --           yield "ERROR! Error with opening \""
      --           yield byteStringFilePath
      --           yield "\" as a file or a directory"
      --         Nothing -> do
      --           yield "ERROR! Error with opening \""
      --           yield byteStringFilePath
      --           yield "\" as a file"
    g (fileNumber, whereDid, Right producer) = do
      let filePath = getFilePathFromWhereDid whereDid
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

data FilenameHandlingFromFiles
  = FromFilesNoFilename
  | FromFilesPrintFilename
  deriving (Eq, Read, Show)

computeFilenameHandlingFromFiles
  :: forall a m r.
     Monad m
  => Producer (WhereDidFileComeFrom, a) m r
  -> m FilenameHandlingFromFiles
computeFilenameHandlingFromFiles producer = do
  eitherFirstFile <- next producer
  case eitherFirstFile of
    Left _ -> pure FromFilesNoFilename
    Right ((whereDid, _), producer2) ->
      case whereDid of
        FileSpecifiedByUser _ -> do
          eitherSecondFile <- next producer2
          case eitherSecondFile of
            Left _ -> pure FromFilesNoFilename
            Right (_, _) -> pure FromFilesPrintFilename
        FileFoundRecursively _ -> pure FromFilesPrintFilename
