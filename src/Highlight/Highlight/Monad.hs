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
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Pipes (Pipe, Producer, (>->), await, each, for, yield)

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
import Highlight.Common.Pipes (numberedProducer)
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

handleInputData
  :: (FilenameHandlingFromStdin -> ByteString -> HighlightM [ByteString])
  -> ( FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> [ByteString]
     )
  -> (ByteString -> IOException -> Maybe IOException -> [ByteString])
  -> InputData HighlightM ()
  -> Producer Output HighlightM ()
handleInputData stdinFunc _ _ (InputDataStdin producer) =
  handleInputDataStdin stdinFunc producer
handleInputData _ handleNonError handleError (InputDataFile filenameHandling fileProducer) =
  handleInputDataFile handleNonError handleError filenameHandling fileProducer

handleInputData'
  :: (FilenameHandlingFromStdin -> ByteString -> HighlightM [ByteString])
  -> ( FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> [ByteString]
     )
  -> (ByteString -> IOException -> Maybe IOException -> [ByteString])
  -> InputData' HighlightM ()
  -> Producer Output HighlightM ()
-- handleInputData' stdinFunc _ _ (InputDataStdin producer) =
--   handleInputDataStdin stdinFunc producer
handleInputData' stdinF nonErrF errF (InputData' nameHandling producer) = do
  -- handleInputDataFile handleNonError handleError nameHandling producer
  stdinHandling <- filenameHandlingFromStdinM
  go stdinHandling
  where
    go :: FilenameHandlingFromStdin -> Producer Output HighlightM ()
    go stdinHandling = producer >-> f
      where
        f :: Pipe (FileReader ByteString) Output HighlightM ()
        f = do
          fileReader <- await
          let maybeFileOrigin = isFileReaderStdin fileReader
          case maybeFileOrigin of
            True ->
              case fileReader of
                FileReaderSuccess _ line -> do
                  outByteStrings <- stdinF stdinHandling line
                  sendToStdoutWithNewLine outByteStrings
                -- We should never have an error reading from stdin, so just
                -- do nothing.
                FileReaderErr _ _ _ -> return ()
            False -> undefined
          f

-- TODO: Make sure this can compile.


sendToStdoutWithNewLine :: Monad m => [ByteString] -> Producer' Output m ()
sendToStdoutWithNewLine byteStrings = do
  let outputs = fmap OutputStdout byteStrings
  case outputs of
    [] -> return ()
    (_:_) -> do
      each outputs
      yield $ OutputStdout "\n"

-- data InputData' m a
--   = InputData'
--       !FilenameHandlingFromFiles
--       !(Producer (FileReader ByteString) m ())
--
-- data FileReader a
--   = FileReaderSuccess !FileOrigin !a
--   | FileReaderErr !FileOrigin !IOException !(Maybe IOException)
--   deriving (Eq, Show)
--
-- data FileOrigin
--   = FileSpecifiedByUser FilePath
--   | FileFoundRecursively FilePath
--   | StdIn
--   deriving (Eq, Read, Show)
--
-- getFilePathFromFileOrigin :: FileOrigin -> Maybe FilePath
--
-- fileOriginToString :: FileOrigin -> String


handleInputDataStdin
  :: ( FilenameHandlingFromStdin
        -> ByteString
        -> HighlightM [ByteString]
     )
  -> Producer ByteString HighlightM ()
  -> Producer Output HighlightM ()
handleInputDataStdin f producer = do
  filenameHandling <- filenameHandlingFromStdinM
  producer >-> addNewline (f filenameHandling)
  where
    addNewline
      :: forall m. Monad m
      => (ByteString -> m [ByteString])
      -> Pipe ByteString Output m ()
    addNewline func = go
      where
        go :: Pipe ByteString Output m ()
        go = do
          inputLine <- await
          modifiedLine <- lift $ func inputLine >>= return . fmap OutputStdout
          case modifiedLine of
            [] -> go
            (_:_) -> do
              each modifiedLine
              yield $ OutputStdout "\n"
              go

handleInputDataFile
  :: ( FilenameHandlingFromFiles
        -> ByteString
        -> Int
        -> ByteString
        -> [ByteString]
     )
  -> (ByteString -> IOException -> Maybe IOException -> [ByteString])
  -> FilenameHandlingFromFiles
  -> FileProducer HighlightM ()
  -> Producer Output HighlightM ()
handleInputDataFile handleNonError handleError filenameHandling fileProducer =
  for (numberedProducer fileProducer) g
  where
    g
      :: ( Int
         , FileOrigin
         , Either
            (IOException, Maybe IOException)
            (Producer ByteString HighlightM ())
         )
      -> Producer Output HighlightM ()
    g (_, fileOrigin, Left (ioerr, maybeioerr)) = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      byteStringFilePath <- convertStringToRawByteString filePath
      let outputLines =
            fmap OutputStderr $ handleError byteStringFilePath ioerr maybeioerr
      each outputLines
      yield $ OutputStderr "\n"
    g (fileNumber, fileOrigin, Right producer) = do
      let filePath = getFilePathFromFileOrigin fileOrigin
      byteStringFilePath <- convertStringToRawByteString filePath
      producer >-> bababa byteStringFilePath
      where
        bababa :: ByteString -> Pipe ByteString Output HighlightM ()
        bababa filePath = go
          where
            go :: Pipe ByteString Output HighlightM ()
            go = do
              inputLine <- await
              let outputLines =
                    fmap OutputStdout $
                      handleNonError
                        filenameHandling
                        filePath
                        fileNumber
                        inputLine
              case outputLines of
                [] -> go
                (_:_) -> do
                  each outputLines
                  yield $ OutputStdout "\n"
                  go

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
