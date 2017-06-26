{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Highlight.Monad
  ( module Highlight.Highlight.Monad
  , module Highlight.Common.Monad
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException)
import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, StateT, evalStateT, get, modify', put)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Pipes (Pipe, Producer, Producer', Proxy, (>->), await, each, yield)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad
       -- (CommonHighlightM,
       --  FilenameHandlingFromFiles(NoFilename, PrintFilename),
       --  FileOrigin(FileFoundRecursively, FileSpecifiedByUser),
       --  FileProducer, InputData(InputDataFile, InputDataStdin),
       --  Output(OutputStderr, OutputStdout), compileHighlightRegexWithErr,
       --  computeFilenameHandlingFromFiles, createInputData,
       --  getFilePathFromFileOrigin, getIgnoreCaseM, getInputFilenamesM,
       --  getRawRegexM, getRecursiveM, outputConsumer, produerForSingleFile,
       --  runCommonHighlightM, throwRegexCompileErr)
import Highlight.Common.Util (convertStringToRawByteString)
import Highlight.Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        HasColorGrepFilenames(colorGrepFilenamesLens), Options(..))

data FromGrepFilenameState = FromGrepFilenameState
  { fromGrepFilenameStatePrevFileNum :: {-# UNPACK #-} !Int
  , fromGrepFilenameStatePrevFilename :: !(Maybe ByteString)
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
  :: (HasColorGrepFilenames r, MonadReader r m) => m ColorGrepFilenames
getColorGrepFilenamesM = view colorGrepFilenamesLens

-----------
-- Pipes --
-----------

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

whenNonNull :: Monad m => [a] -> m () -> m ()
whenNonNull [] _ = return ()
whenNonNull _ action = action
{-# INLINABLE whenNonNull #-}

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

filenameHandlingFromStdinM
  :: (HasColorGrepFilenames r, MonadReader r m)
  => m FilenameHandlingFromStdin
filenameHandlingFromStdinM =
  fmap computeFilenameHandlingFromStdin getColorGrepFilenamesM
