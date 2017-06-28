{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Common.Monad.Output
  ( Output(..)
  , handleInputData
  , outputConsumer
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, evalStateT, get)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Pipes
       (Consumer, Pipe, Producer, Producer', Proxy, (>->), await, each,
        yield)
import Pipes.ByteString (stdout)

import Highlight.Common.Monad.Input
       (FilenameHandlingFromFiles, FileOrigin,
        FileReader(FileReaderErr, FileReaderSuccess),
        InputData(InputData), getFileOriginFromFileReader,
        getFilePathFromFileReader)
import Highlight.Pipes (stderrConsumer)
import Highlight.Util
       (convertStringToRawByteString, modify', whenNonNull)

-----------------------------
-- Convert input to Output --
-----------------------------

type ColorNum = Int

data FileColorState
  = FileColorState !(Maybe FileOrigin) {-# UNPACK #-} !ColorNum
  deriving (Eq, Read, Show)

defFileColorState :: FileColorState
defFileColorState = FileColorState Nothing 0

-- | Convert 'InputData' to 'Output'.
handleInputData
  :: forall m.
     MonadIO m
  => (ByteString -> m [ByteString])
  -> (FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> m [ByteString]
     )
  -> (ByteString -> IOException -> Maybe IOException -> m [ByteString])
  -> InputData m ()
  -> Producer Output m ()
handleInputData stdinF nonErrF errF (InputData nameHandling producer) =
  producer >-> evalStateT go defFileColorState
  where
    go :: StateT FileColorState (Pipe (FileReader ByteString) Output m) ()
    go = do
      fileReader <- lift await
      let maybeFilePath = getFilePathFromFileReader fileReader
          fileOrigin = getFileOriginFromFileReader fileReader
      colorNumIfNewFile <- getColorNumIfNewFileM fileOrigin
      case maybeFilePath of
        -- The current @'FileReader' 'ByteString'@ is from stdin.
        Nothing ->
          case fileReader of
            FileReaderSuccess _ line -> do
              outByteStrings <- lift . lift $ stdinF line
              sendToStdoutWhenNonNull outByteStrings fileOrigin
            FileReaderErr _ _ _ -> return ()
        -- The current @'FileReader' 'ByteString'@ is from a normal file.
        Just filePath -> do
          byteStringFilePath <- convertStringToRawByteString filePath
          case fileReader of
            FileReaderErr _ ioerr maybeioerr -> do
              outByteStrings <-
                lift . lift $ errF byteStringFilePath ioerr maybeioerr
              sendToStderrWhenNonNull outByteStrings
            FileReaderSuccess _ inputLine -> do
              outByteStrings <-
                lift . lift $
                  nonErrF
                    nameHandling
                    byteStringFilePath
                    colorNumIfNewFile
                    inputLine
              sendToStdoutWhenNonNull outByteStrings fileOrigin
      go

sendToStdoutWhenNonNull
  :: Monad m
  => [ByteString]
  -> FileOrigin
  -> StateT FileColorState (Proxy x' x () Output m) ()
sendToStdoutWhenNonNull outByteStrings fileOrigin =
  whenNonNull outByteStrings $ do
    lift $ sendToStdoutWithNewLine outByteStrings
    updateColorNumM fileOrigin

sendToStderrWhenNonNull
  :: Monad m => [ByteString] -> StateT s (Proxy x' x () Output m) ()
sendToStderrWhenNonNull outByteStrings =
  whenNonNull outByteStrings . lift $ sendToStderrWithNewLine outByteStrings

updateColorNumM
  :: MonadState FileColorState m => FileOrigin -> m ()
updateColorNumM = modify' . updateColorNum

updateColorNum
  :: FileOrigin
  -> FileColorState
  -> FileColorState
updateColorNum newFileOrigin (FileColorState Nothing colorNum) =
  FileColorState (Just newFileOrigin) colorNum
updateColorNum newFileOrigin (FileColorState (Just prevFileOrigin) colorNum)
  | prevFileOrigin == newFileOrigin =
    FileColorState (Just newFileOrigin) colorNum
  | otherwise = FileColorState (Just newFileOrigin) (colorNum + 1)

getColorNumIfNewFileM
  :: MonadState FileColorState m
  => FileOrigin -> m ColorNum
getColorNumIfNewFileM newFileOrigin = do
  fileColorState <- get
  let newColorNum = getColorNumIfNewFile newFileOrigin fileColorState
  return newColorNum

getColorNumIfNewFile :: FileOrigin -> FileColorState -> ColorNum
getColorNumIfNewFile _ (FileColorState Nothing colorNum) = colorNum
getColorNumIfNewFile newFileOrigin (FileColorState (Just prevFileOrigin) colorNum)
  | prevFileOrigin == newFileOrigin = colorNum
  | otherwise = colorNum + 1


sendToStdoutWithNewLine :: Monad m => [ByteString] -> Producer' Output m ()
sendToStdoutWithNewLine = sendWithNewLine OutputStdout

sendToStderrWithNewLine :: Monad m => [ByteString] -> Producer' Output m ()
sendToStderrWithNewLine = sendWithNewLine OutputStderr

sendWithNewLine
  :: Monad m
  => (ByteString -> Output) -> [ByteString] -> Producer' Output m ()
sendWithNewLine byteStringToOutput byteStrings = do
  let outputs = fmap byteStringToOutput byteStrings
  case outputs of
    [] -> return ()
    (_:_) -> do
      each outputs
      yield $ byteStringToOutput "\n"

------------
-- Output --
------------

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
