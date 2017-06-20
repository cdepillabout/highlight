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
import Highlight.Common.Options
       (HasIgnoreCase(getIgnoreCase),
        HasInputFilenames(getInputFilenames), HasRecursive(getRecursive),
        HasRawRegex(getRawRegex), IgnoreCase, InputFilename, RawRegex,
        Recursive(Recursive))
import Highlight.Common.Pipes
       (childOf, fromHandleLines, numberedProducer, stderrConsumer)
import Highlight.Common.Util
       (combineApplicatives, convertStringToRawByteString,
        openFilePathForReading)

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
getIgnoreCaseM  = reader getIgnoreCase

getRecursiveM :: (HasRecursive r, MonadReader r m) => m Recursive
getRecursiveM = reader getRecursive

getRawRegexM :: (HasRawRegex r, MonadReader r m) => m RawRegex
getRawRegexM = reader getRawRegex

getInputFilenamesM
  :: (HasInputFilenames r, MonadReader r m)
  => m [InputFilename]
getInputFilenamesM = reader getInputFilenames

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
              let producer = childOf filePath
                  lIO = runSafeT $ toListM producer
              eitherFileList <- liftIO (try lIO)
              case eitherFileList of
                Left dirIOErr -> do
                  yield (fileOrigin, Left (fileIOErr, Just dirIOErr))
                Right fileList -> do
                  let sortedFileList = sort fileList
                  let fileOrigin = fmap FileFoundRecursively sortedFileList
                  let lalas =
                        fmap
                          (produerForSingleFile recursive)
                          fileOrigin
                  for (each lalas) id
            else
              yield (fileOrigin, Left (fileIOErr, Nothing))

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
