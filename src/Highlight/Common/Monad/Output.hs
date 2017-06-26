{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Common.Monad.Output where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException)
import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, StateT, evalStateT, get, modify', put)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Pipes
       (Consumer, Pipe, Producer, Producer', Proxy, (>->), await, each,
        yield)
import Pipes.ByteString (stdout)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad.Input
import Highlight.Common.Pipes (stderrConsumer)
import Highlight.Common.Util (convertStringToRawByteString, whenNonNull)
import Highlight.Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        HasColorGrepFilenames(colorGrepFilenamesLens), Options(..))

-----------------------------
-- Convert input to Output --
-----------------------------

handleInputData'
  :: forall m.
     MonadIO m
  => (ByteString -> m [ByteString])
  -> (FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> [ByteString]
     )
  -> (ByteString -> IOException -> Maybe IOException -> [ByteString])
  -> InputData' m ()
  -> Producer Output m ()
handleInputData' stdinF nonErrF errF (InputData' nameHandling producer) =
  producer >-> evalStateT go (Nothing, 0)
  where
    go
      :: StateT
          (Maybe FileOrigin, ColorNum)
          (Pipe (FileReader ByteString) Output m)
          ()
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
              let outByteStrings = errF byteStringFilePath ioerr maybeioerr
              sendToStderrWhenNonNull outByteStrings
            FileReaderSuccess _ inputLine -> do
              let outByteStrings =
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
  -> StateT (Maybe FileOrigin, ColorNum) (Proxy x' x () Output m) ()
sendToStdoutWhenNonNull outByteStrings fileOrigin =
  whenNonNull outByteStrings $ do
    lift $ sendToStdoutWithNewLine outByteStrings
    updateColorNumM fileOrigin

sendToStderrWhenNonNull
  :: Monad m => [ByteString] -> StateT s (Proxy x' x () Output m) ()
sendToStderrWhenNonNull outByteStrings =
  whenNonNull outByteStrings . lift $ sendToStderrWithNewLine outByteStrings

updateColorNumM
  :: MonadState (Maybe FileOrigin, ColorNum) m => FileOrigin -> m ()
updateColorNumM = modify' . updateColorNum

updateColorNum
  :: FileOrigin
  -> (Maybe FileOrigin, ColorNum)
  -> (Maybe FileOrigin, ColorNum)
updateColorNum newFileOrigin (Nothing, colorNum) = (Just newFileOrigin, colorNum)
updateColorNum newFileOrigin (Just prevFileOrigin, colorNum)
  | prevFileOrigin == newFileOrigin = (Just newFileOrigin, colorNum)
  | otherwise = (Just newFileOrigin, colorNum + 1)

getColorNumIfNewFileM
  :: MonadState (Maybe FileOrigin, ColorNum) m
  => FileOrigin -> m ColorNum
getColorNumIfNewFileM newFileOrigin = do
  (maybePrevFileOrigin, prevColorNum) <- get
  let newColorNum =
        getColorNumIfNewFile maybePrevFileOrigin prevColorNum newFileOrigin
  return newColorNum

getColorNumIfNewFile
  :: Maybe FileOrigin
  -> ColorNum
  -> FileOrigin
  -> ColorNum
getColorNumIfNewFile Nothing colorNum _ = colorNum
getColorNumIfNewFile (Just prevFileOrigin) colorNum newFileOrigin
  | prevFileOrigin == newFileOrigin = colorNum
  | otherwise = colorNum + 1

type ColorNum = Int

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
