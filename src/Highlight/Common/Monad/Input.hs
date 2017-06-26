{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Highlight.Common.Monad.Input where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.List (sort)
import Pipes
       (Pipe, Producer, Producer', Proxy, (>->), await, each, for, next,
        yield)
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude as Pipes
import System.IO (Handle)

import Highlight.Common.Options
       (InputFilename(unInputFilename), Recursive(Recursive))
import Highlight.Common.Pipes (childOf, fromHandleLines)
import Highlight.Common.Util
       (combineApplicatives, openFilePathForReading)

-----------
-- Pipes --
-----------

data FileOrigin
  = FileSpecifiedByUser FilePath
  | FileFoundRecursively FilePath
  | Stdin
  deriving (Eq, Read, Show)

getFilePathFromFileOrigin :: FileOrigin -> Maybe FilePath
getFilePathFromFileOrigin (FileSpecifiedByUser fp) = Just fp
getFilePathFromFileOrigin (FileFoundRecursively fp) = Just fp
getFilePathFromFileOrigin Stdin = Nothing

fileOriginToString :: FileOrigin -> String
fileOriginToString (FileSpecifiedByUser fp) = fp
fileOriginToString (FileFoundRecursively fp) = fp
fileOriginToString Stdin = "(standard input)"

isFileOriginStdin :: FileOrigin -> Bool
isFileOriginStdin Stdin = True
isFileOriginStdin _ = False

data FileReader a
  = FileReaderSuccess !FileOrigin !a
  | FileReaderErr !FileOrigin !IOException !(Maybe IOException)
  deriving (Eq, Show)

getFileOriginFromFileReader :: FileReader a -> FileOrigin
getFileOriginFromFileReader (FileReaderSuccess origin _) = origin
getFileOriginFromFileReader (FileReaderErr origin _ _) = origin

getFilePathFromFileReader :: FileReader a -> Maybe FilePath
getFilePathFromFileReader =
  getFilePathFromFileOrigin . getFileOriginFromFileReader

isFileReaderStdin :: FileReader a -> Bool
isFileReaderStdin = isFileOriginStdin . getFileOriginFromFileReader

data InputData m a
  = InputData
      !FilenameHandlingFromFiles
      !(Producer (FileReader ByteString) m ())

createInputData
  :: forall m.
     MonadIO m
  => Recursive
  -> [InputFilename]
  -> Producer ByteString m ()
  -> m (InputData m ())
createInputData recursive inputFilenames stdinProducer = do
  let fileOrigins = FileSpecifiedByUser . unInputFilename <$> inputFilenames
  case fileOrigins of
    [] ->
      return $
        InputData NoFilename (stdinProducerToFileReader stdinProducer)
    _ -> do
      let fileListProducers = fmap (fileListProducer recursive) fileOrigins
          fileProducer = foldl1 combineApplicatives fileListProducers
      (filenameHandling, newFileProducer) <-
        computeFilenameHandlingFromFiles fileProducer
      let fileLineProducer = fileReaderHandleToLine newFileProducer
      return $ InputData filenameHandling fileLineProducer

stdinProducerToFileReader
  :: forall x' x a m r.
     Monad m
  => Proxy x' x () a m r
  -> Proxy x' x () (FileReader a) m r
stdinProducerToFileReader producer = producer >-> Pipes.map go
  where
    go :: a -> FileReader a
    go = FileReaderSuccess Stdin
{-# INLINABLE stdinProducerToFileReader #-}

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
      let maybeFilePath = getFilePathFromFileOrigin fileOrigin
      case maybeFilePath of
        -- This is standard input.  We don't currently handle this, so just
        -- return unit.
        Nothing -> return ()
        -- This is a normal file.  Not standard input.
        Just filePath -> do
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
  => Producer (FileReader a) m r
  -> m (FilenameHandlingFromFiles, Producer (FileReader a) m r)
computeFilenameHandlingFromFiles producer1 = do
  eitherFileReader1 <- next producer1
  case eitherFileReader1 of
    Left ret ->
      return (NoFilename, return ret)
    Right (fileReader1, producer2) -> do
      let fileOrigin1 = getFileOriginFromFileReader fileReader1
      case fileOrigin1 of
        Stdin -> error "Not currenty handling stdin..."
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
