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

-- | The current number of the file we are outputting.  This is increased by
-- one for each file.
type ColorNum = Int

-- | The state for which number file we are outputting with its 'FileOrigin'.
data FileColorState
  = FileColorState !(Maybe FileOrigin) {-# UNPACK #-} !ColorNum
  deriving (Eq, Read, Show)

-- | Initial value for 'FileColorState'. Defined as the following:
--
-- >>> defFileColorState
-- FileColorState Nothing 0
defFileColorState :: FileColorState
defFileColorState = FileColorState Nothing 0

-- | Convert 'InputData' to 'Output'.
handleInputData
  :: forall m.
     MonadIO m
  => (ByteString -> m [ByteString])
  -- ^ Function to use for conversion for a line from stdin.
  -> (FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> m [ByteString]
     )
  -- ^ Function to use for conversion for a line from a normal file.
  -> (ByteString -> IOException -> Maybe IOException -> m [ByteString])
  -- ^ Function to use for conversion for an io error.
  -> InputData m ()
  -- ^ All of the input lines.
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

-- | When the list of 'ByteString' is not @[]@, convert them to 'OutputStdout'
-- and return them in a 'Producer'.  Call 'updateColorNum' with the passed-in
-- 'FileOrigin'.
toStdoutWhenNonNull
  :: forall m x' x.
     Monad m
  => [ByteString]
  -> FileOrigin
  -> StateT FileColorState (Proxy x' x () Output m) ()
toStdoutWhenNonNull outByteStrings fileOrigin =
  whenNonNull outByteStrings $ do
    lift $ toStdoutWithNewline outByteStrings
    updateColorNumM fileOrigin

-- | When the list of 'ByteString' is not @[]@, convert them to 'OutputStderr'
-- and return them in a 'Producer'.  Do not call 'updateColorNum'.
toStderrWhenNonNull
  :: forall m s x' x.
     Monad m
  => [ByteString] -> StateT s (Proxy x' x () Output m) ()
toStderrWhenNonNull outByteStrings =
  whenNonNull outByteStrings . lift $ toStderrWithNewline outByteStrings

-- | Call 'updateColorNum' with the current value of 'FileColorState'.
updateColorNumM
  :: MonadState FileColorState m => FileOrigin -> m ()
updateColorNumM = modify' . updateColorNum

-- | Based on the value of the passed in 'FileOrigin' and the 'FileOrigin' in
-- 'FileColorState', update the 'ColorNum' in 'FileColorState'.
--
-- Setup for examples:
--
-- >>> import Highlight.Common.Monad.Input (FileOrigin(FileSpecifiedByUser))
--
-- If 'defFileColorState' is passed in, then update the 'FileOrigin'.
--
-- >>> let fileOrigin = FileSpecifiedByUser "hello.txt"
-- >>> updateColorNum fileOrigin defFileColorState
-- FileColorState (Just (FileSpecifiedByUser "hello.txt")) 0
--
-- If the 'FileOrigin' is different from the one in 'FileColorState', then
-- increment the 'ColorNum'.
--
-- >>> let oldFileOrigin = FileSpecifiedByUser "hello.txt"
-- >>> let newFileOrigin = FileSpecifiedByUser "bye.txt"
-- >>> let fileColorStateDiff = FileColorState (Just oldFileOrigin) 5
-- >>> updateColorNum newFileOrigin fileColorStateDiff
-- FileColorState (Just (FileSpecifiedByUser "bye.txt")) 6
--
-- If the 'FileOrigin' is the same as the one in 'FileColorState', then do not
-- increment the 'ColorNum'.
--
-- >>> let sameFileOrigin = FileSpecifiedByUser "hello.txt"
-- >>> let fileColorStateDiff = FileColorState (Just sameFileOrigin) 5
-- >>> updateColorNum sameFileOrigin fileColorStateDiff
-- FileColorState (Just (FileSpecifiedByUser "hello.txt")) 5
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

-- | This called 'updateColorNum' and returns the new value of 'ColorNum', but
-- it doesn't update the 'FileColorNum' in 'MonadState'.
getColorNumIfNewFileM
  :: MonadState FileColorState m
  => FileOrigin -> m ColorNum
getColorNumIfNewFileM newFileOrigin = do
  fileColorState <- get
  let (FileColorState _ colorNum) = updateColorNum newFileOrigin fileColorState
  return colorNum

-- | This is just 'toOutputWithNewLine' 'OutputStderr'.
toStderrWithNewline :: Monad m => [ByteString] -> Producer' Output m ()
toStderrWithNewline = toOutputWithNewline OutputStderr

-- | This is just 'toOutputWithNewLine' 'OutputStdout'.
toStdoutWithNewline :: Monad m => [ByteString] -> Producer' Output m ()
toStdoutWithNewline = toOutputWithNewline OutputStdout

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

-- | Sum-type to represent where a given 'ByteString' should be output, whether
-- it is stdout or stderr.
data Output
  = OutputStdout !ByteString
  | OutputStderr !ByteString
  deriving (Eq, Read, Show)

-- | Write each 'Output' to either 'stdout' or 'stderrConsumer'.
--
-- Setup for tests:
--
-- >>> import Pipes (runEffect)
--
-- Send 'OutputStdout' to 'stdout'.
--
-- >>> runEffect $ yield (OutputStdout "this goes to stdout") >-> outputConsumer
-- this goes to stdout
--
-- Send 'OutputStderr' to 'stderrConsumer'.
--
-- >>> runEffect $ yield (OutputStderr "this goes to stderr") >-> outputConsumer
-- this goes to stderr
outputConsumer :: MonadIO m => Consumer Output m ()
outputConsumer = do
  output <- await
  case output of
    OutputStdout byteString ->
      yield byteString >-> stdout
    OutputStderr byteString ->
      yield byteString >-> stderrConsumer
  outputConsumer
