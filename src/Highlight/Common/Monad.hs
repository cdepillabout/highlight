{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Highlight.Common.Monad where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException, try)
import Control.Lens (view)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.ByteString (ByteString)
import Data.List (sort)
import Pipes
       (Consumer, Pipe, Producer, Producer', Proxy, (>->), await, each, for,
        next, yield)
import Pipes.ByteString (stdout)
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude as Pipes
import System.IO (Handle)
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        compileRegexWith)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Options
       (HasIgnoreCase(ignoreCaseLens),
        HasInputFilenames(inputFilenamesLens), HasRecursive(recursiveLens),
        HasRawRegex(rawRegexLens), IgnoreCase(DoNotIgnoreCase, IgnoreCase),
        InputFilename(unInputFilename), RawRegex(RawRegex),
        Recursive(Recursive))
import Highlight.Common.Pipes
       (childOf, fromHandleLines, stderrConsumer)
import Highlight.Common.Util
       (combineApplicatives, openFilePathForReading)

--------------------------------
-- The Common Highlight Monad --
--------------------------------

newtype CommonHighlightM r s e a = CommonHighlightM
  { unCommonHighlightM :: ReaderT r (StateT s (ExceptT e IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError e
             , MonadIO
             , MonadReader r
             , MonadState s
             )

runCommonHighlightM :: r -> s -> CommonHighlightM r s e a -> IO (Either e a)
runCommonHighlightM r s =
  runExceptT .
    flip evalStateT s .
    flip runReaderT r .
    unCommonHighlightM

getIgnoreCaseM :: (HasIgnoreCase r, MonadReader r m) => m IgnoreCase
getIgnoreCaseM  = view ignoreCaseLens

getRecursiveM :: (HasRecursive r, MonadReader r m) => m Recursive
getRecursiveM = view recursiveLens

getRawRegexM :: (HasRawRegex r, MonadReader r m) => m RawRegex
getRawRegexM = view rawRegexLens

getInputFilenamesM
  :: (HasInputFilenames r, MonadReader r m) => m [InputFilename]
getInputFilenamesM = view inputFilenamesLens

------------------
-- Throw Errors --
------------------

throwHighlightErr :: HighlightErr -> CommonHighlightM r s HighlightErr a
throwHighlightErr = throwError

throwRegexCompileErr :: RawRegex -> CommonHighlightM r s HighlightErr a
throwRegexCompileErr = throwHighlightErr . HighlightRegexCompileErr

-----------
-- Pipes --
-----------

type FileProducer m a =
  Producer
    ( FileOrigin
    , Either
        (IOException, Maybe IOException)
        (Producer ByteString m a)
    )
    m
    a

data FileOrigin
  = FileSpecifiedByUser FilePath
  | FileFoundRecursively FilePath
  deriving (Eq, Read, Show)

getFilePathFromFileOrigin :: FileOrigin -> FilePath
getFilePathFromFileOrigin (FileSpecifiedByUser fp) = fp
getFilePathFromFileOrigin (FileFoundRecursively fp) = fp

createInputData
  :: forall r s e.
     (HasInputFilenames r, HasRecursive r)
  => Producer ByteString (CommonHighlightM r s e) ()
  -> CommonHighlightM r s e (InputData (CommonHighlightM r s e) ())
createInputData stdinProducer = do
  inputFilenames <-
    fmap (FileSpecifiedByUser . unInputFilename) <$> getInputFilenamesM
  recursive <- getRecursiveM
  case inputFilenames of
    [] -> return $ InputDataStdin stdinProducer
    files -> do
      let lalas = fmap (produerForSingleFile recursive) files
      let fileProducer = foldl1 combineApplicatives lalas
      (filenameHandling, newHighlightFileProducer) <-
        computeFilenameHandlingFromFiles fileProducer
      return $ InputDataFile filenameHandling newHighlightFileProducer

data InputData m a
  = InputDataStdin !(Producer ByteString m a)
  | InputDataFile !FilenameHandlingFromFiles !(FileProducer m a)

data FileReader a
  = FileReaderSuccess !FileOrigin !a
  | FileReaderErr !FileOrigin !IOException !(Maybe IOException)
  deriving (Eq, Show)

getFileOriginFromFileReader :: FileReader a -> FileOrigin
getFileOriginFromFileReader (FileReaderSuccess origin _) = origin
getFileOriginFromFileReader (FileReaderErr origin _ _) = origin

fileReaderHandleToLine
  :: forall m x' x.
     MonadIO m
  => Proxy x' x () (FileReader Handle) m ()
  -> Proxy x' x () (FileReader ByteString) m ()
fileReaderHandleToLine producer = producer >-> pipe
  where
    pipe :: Pipe (FileReader Handle) (FileReader ByteString) m ()
    pipe = do
      fileReaderHandle <- await
      case fileReaderHandle of
        FileReaderErr fileOrigin fileErr dirErr ->
          yield $ FileReaderErr fileOrigin fileErr dirErr
        FileReaderSuccess fileOrigin handle -> do
          let linesProducer = fromHandleLines handle
          linesProducer >-> Pipes.map (FileReaderSuccess fileOrigin)
      pipe

fileListProducer
  :: forall m.
     MonadIO m
  => Recursive
  -> FileOrigin
  -> Producer' (FileReader Handle) m ()
fileListProducer recursive = go
  where
    go :: FileOrigin -> Producer' (FileReader Handle) m ()
    go fileOrigin = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      eitherHandle <- openFilePathForReading filePath
      case eitherHandle of
        Right handle -> yield $ FileReaderSuccess fileOrigin handle
        Left fileIOErr ->
          if recursive == Recursive
            then do
              let fileListM = toListM $ childOf filePath
              eitherFileList <- liftIO $ try fileListM
              case eitherFileList of
                Left dirIOErr ->
                  yield $
                    FileReaderErr fileOrigin fileIOErr (Just dirIOErr)
                Right fileList -> do
                  let sortedFileList = sort fileList
                  let fileOrigins = fmap FileFoundRecursively sortedFileList
                  let lalas =
                        fmap
                          (fileListProducer recursive)
                          fileOrigins
                  for (each lalas) id
            else
              yield $ FileReaderErr fileOrigin fileIOErr Nothing

-- | TODO: It would be nice to turn this into two functions, one that just gets
-- a list of all files to read, and one that creates the 'Producer' that
-- actually pulles lines out of the file.
produerForSingleFile
  :: forall m.
     MonadIO m
  => Recursive -> FileOrigin -> FileProducer m ()
produerForSingleFile recursive = go
  where
    go :: FileOrigin -> FileProducer m ()
    go fileOrigin = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      eitherHandle <- openFilePathForReading filePath
      case eitherHandle of
        Right handle -> do
          let linesProducer = fromHandleLines handle
          yield (fileOrigin, Right linesProducer)
        Left fileIOErr ->
          if recursive == Recursive
            then do
              let fileListM = toListM $ childOf filePath
              eitherFileList <- liftIO $ try fileListM
              case eitherFileList of
                Left dirIOErr ->
                  yield (fileOrigin, Left (fileIOErr, Just dirIOErr))
                Right fileList -> do
                  let sortedFileList = sort fileList
                  let fileOrigins = fmap FileFoundRecursively sortedFileList
                  let lalas =
                        fmap
                          (produerForSingleFile recursive)
                          fileOrigins
                  for (each lalas) id
            else
              yield (fileOrigin, Left (fileIOErr, Nothing))


data Output
  = OutputStdout !ByteString
  | OutputStderr !ByteString
  deriving (Eq, Read, Show)

outputConsumer :: MonadIO m => Consumer Output m ()
outputConsumer = do
  output <- await
  case output of
    OutputStdout byteString ->
      yield byteString >-> stdout
    OutputStderr byteString ->
      yield byteString >-> stderrConsumer
  outputConsumer

-----------------------
-- Filename Handling --
-----------------------

data FilenameHandlingFromFiles
  = NoFilename
  | PrintFilename
  deriving (Eq, Read, Show)

computeFilenameHandlingFromFiles
  :: forall a m r.
     Monad m
  => Producer (FileOrigin, a) m r
  -> m (FilenameHandlingFromFiles, Producer (FileOrigin, a) m r)
computeFilenameHandlingFromFiles producer = do
  eitherFirstFile <- next producer
  case eitherFirstFile of
    Left ret ->
      return (NoFilename, return ret)
    Right ((fileOrigin1, a1), producer2) ->
      case fileOrigin1 of
        FileSpecifiedByUser _ -> do
          eitherSecondFile <- next producer2
          case eitherSecondFile of
            Left ret2 ->
              return (NoFilename, yield (fileOrigin1, a1) *> return ret2)
            Right ((fileOrigin2, a2), producer3) ->
              return
                ( PrintFilename
                , yield (fileOrigin1, a1) *> yield (fileOrigin2, a2) *> producer3
                )
        FileFoundRecursively _ ->
          return (PrintFilename, yield (fileOrigin1, a1) *> producer2)

computeFilenameHandlingFromFiles'
  :: forall a m r.
     Monad m
  => Producer (FileReader a) m r
  -> m (FilenameHandlingFromFiles, Producer (FileReader a) m r)
computeFilenameHandlingFromFiles' producer1 = do
  eitherFileReader1 <- next producer1
  case eitherFileReader1 of
    Left ret ->
      return (NoFilename, return ret)
    Right (fileReader1, producer2) -> do
      let fileOrigin1 = getFileOriginFromFileReader fileReader1
      case fileOrigin1 of
        FileSpecifiedByUser _ -> do
          eitherSecondFile <- next producer2
          case eitherSecondFile of
            Left ret2 ->
              return (NoFilename, yield fileReader1 *> return ret2)
            Right (fileReader2, producer3) ->
              return
                ( PrintFilename
                , yield fileReader1 *> yield fileReader2 *> producer3
                )
        FileFoundRecursively _ ->
          return (PrintFilename, yield fileReader1 *> producer2)

-----------
-- Regex --
-----------

compileHighlightRegexWithErr
  :: (HasIgnoreCase r, HasRawRegex r)
  => CommonHighlightM r s HighlightErr RE
compileHighlightRegexWithErr = do
  ignoreCase <- getIgnoreCaseM
  rawRegex <- getRawRegexM
  case compileHighlightRegex ignoreCase rawRegex of
    Just re -> return re
    Nothing -> throwRegexCompileErr rawRegex

compileHighlightRegex :: IgnoreCase -> RawRegex -> Maybe RE
compileHighlightRegex ignoreCase (RawRegex rawRegex) =
  let simpleREOptions =
        case ignoreCase of
          IgnoreCase -> MultilineInsensitive
          DoNotIgnoreCase -> MultilineSensitive
  in compileRegexWith simpleREOptions rawRegex
