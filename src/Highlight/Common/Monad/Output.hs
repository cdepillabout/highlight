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
              toStdoutWhenNonNull outByteStrings fileOrigin
            FileReaderErr _ _ _ -> return ()
        -- The current @'FileReader' 'ByteString'@ is from a normal file.
        Just filePath -> do
          byteStringFilePath <- convertStringToRawByteString filePath
          case fileReader of
            FileReaderErr _ ioerr maybeioerr -> do
              outByteStrings <-
                lift . lift $ errF byteStringFilePath ioerr maybeioerr
              toStderrWhenNonNull outByteStrings
            FileReaderSuccess _ inputLine -> do
              outByteStrings <-
                lift . lift $
                  nonErrF
                    nameHandling
                    byteStringFilePath
                    colorNumIfNewFile
                    inputLine
              toStdoutWhenNonNull outByteStrings fileOrigin
      go

toStdoutWhenNonNull
  :: Monad m
  => [ByteString]
  -> FileOrigin
  -> StateT FileColorState (Proxy x' x () Output m) ()
toStdoutWhenNonNull outByteStrings fileOrigin =
  whenNonNull outByteStrings $ do
    lift $ toStdoutWithNewline outByteStrings
    updateColorNumM fileOrigin

toStderrWhenNonNull
  :: Monad m => [ByteString] -> StateT s (Proxy x' x () Output m) ()
toStderrWhenNonNull outByteStrings =
  whenNonNull outByteStrings . lift $ toStderrWithNewline outByteStrings

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


toStdoutWithNewline :: Monad m => [ByteString] -> Producer' Output m ()
toStdoutWithNewline = toOutputWithNewline OutputStdout

toStderrWithNewline :: Monad m => [ByteString] -> Producer' Output m ()
toStderrWithNewline = toOutputWithNewline OutputStderr

-- | Convert a list of 'ByteString' to 'Output' with the given function.
--
-- Setup for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Pipes.Prelude (toListM)
--
-- If the list of 'ByteString' is empty, then do nothing.
--
-- >>> toListM $ toOutputWithNewline OutputStdout []
-- []
--
-- If the list of 'ByteString' is not empty, then convert to 'Output' and add
-- an ending newline.
--
-- >>> toListM $ toOutputWithNewline OutputStderr ["hello", "bye"]
-- [OutputStderr "hello",OutputStderr "bye",OutputStderr "\n"]
toOutputWithNewline
  :: Monad m
  => (ByteString -> Output)
  -- ^ Function to use to convert the 'ByteString' to 'Output'.
  -> [ByteString]
  -- ^ List of 'ByteString's to convert to 'Output'.
  -> Producer' Output m ()
toOutputWithNewline _ [] = return ()
toOutputWithNewline byteStringToOutput byteStrings = do
  each $ fmap byteStringToOutput byteStrings
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
