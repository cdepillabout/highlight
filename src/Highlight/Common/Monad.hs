{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
       (Consumer, Producer, (>->), await, each, for, next, yield)
import Pipes.ByteString (stdout)
import Pipes.Prelude (toListM)
import System.IO (Handle)
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        compileRegexWith)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Options
       (HasIgnoreCase(ignoreCaseLens),
        HasInputFilenames(inputFilenamesLens), HasRecursive(recursiveLens),
        HasRawRegex(rawRegexLens), IgnoreCase(DoNotIgnoreCase, IgnoreCase),
        InputFilename, RawRegex(RawRegex), Recursive(Recursive))
import Highlight.Common.Pipes
       (childOf, fromHandleLines, stderrConsumer)
import Highlight.Common.Util (openFilePathForReading)

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

data FileOriginWithHandle
  = FileOriginWithHandleSuccess !FileOrigin !Handle
  | FileOriginWithHandleErr !FileOrigin !IOException !(Maybe IOException)
  deriving (Eq, Show)

fileListProducer
  :: forall m.
     MonadIO m
  => Recursive
  -> FileOrigin
  -> Producer FileOriginWithHandle m ()
fileListProducer recursive = go
  where
    go :: FileOrigin -> Producer FileOriginWithHandle m ()
    go fileOrigin = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      eitherHandle <- openFilePathForReading filePath
      case eitherHandle of
        Right handle -> yield $ FileOriginWithHandleSuccess fileOrigin handle
        Left fileIOErr ->
          if recursive == Recursive
            then do
              let fileListM = toListM $ childOf filePath
              eitherFileList <- liftIO $ try fileListM
              case eitherFileList of
                Left dirIOErr ->
                  yield $
                    FileOriginWithHandleErr
                      fileOrigin
                      fileIOErr
                      (Just dirIOErr)
                Right fileList -> do
                  let sortedFileList = sort fileList
                  let fileOrigins = fmap FileFoundRecursively sortedFileList
                  let lalas =
                        fmap
                          (fileListProducer recursive)
                          fileOrigins
                  for (each lalas) id
            else
              yield $ FileOriginWithHandleErr fileOrigin fileIOErr Nothing

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
