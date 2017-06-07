{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}

module Highlight.Monad where

import Control.Lens ((^.))
import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Free (FreeT)
import Data.ByteString (ByteString)
import Data.DirStream (childOf)
import Pipes (Producer, each, enumerate, yield)
import Pipes.ByteString (fromHandle)
import qualified Pipes.ByteString
import System.IO (Handle, IOMode(ReadMode), openBinaryFile)
import System.IO.Error
       (isAlreadyInUseError, isDoesNotExistError, isPermissionError)

import Highlight.Error (FileErr(..), HighlightErr(..))
import Highlight.Options
       (ColorGrepFilenames, IgnoreCase,
        InputFilename(InputFilename, unInputFilename), Options(..),
        RawRegex, Recursive(NotRecursive, Recursive))


-------------------------
-- The Highlight Monad --
-------------------------

newtype HighlightM a = HighlightM
  { unHighlightM :: ReaderT Options (ExceptT HighlightErr IO) a
  } deriving (Functor, Applicative, Monad)

runHighlightM :: Options -> HighlightM a -> IO (Either HighlightErr a)
runHighlightM opts = runExceptT . flip runReaderT opts . unHighlightM

-- | Only used internally.
newtype HighlightMWithIO a = HighlightMWithIO
  { unHighlightMWithIO :: HighlightM a
  } deriving (Functor, Applicative, Monad)

instance MonadIO HighlightMWithIO where
  liftIO :: IO a -> HighlightMWithIO a
  liftIO = HighlightMWithIO . HighlightM . liftIO

----------------------------------
-- Get value of certain options --
----------------------------------

getOptions :: HighlightM Options
getOptions = HighlightM ask

getIgnoreCase :: HighlightM IgnoreCase
getIgnoreCase  = optionsIgnoreCase <$> getOptions

getRecursive :: HighlightM Recursive
getRecursive = optionsRecursive <$> getOptions

getColorGrepFilenames :: HighlightM ColorGrepFilenames
getColorGrepFilenames = optionsColorGrepFilenames <$> getOptions

getRawRegex :: HighlightM RawRegex
getRawRegex = optionsRawRegex <$> getOptions

getInputFilenames :: HighlightM [InputFilename]
getInputFilenames = optionsInputFilenames <$> getOptions

------------------
-- Throw Errors --
------------------

throwHighlightErr :: HighlightErr -> HighlightM a
throwHighlightErr = HighlightM . throwError

throwRegexCompileErr :: RawRegex -> HighlightM a
throwRegexCompileErr = throwHighlightErr . HighlightRegexCompileErr

-- throwFileErr :: Monad m => FileErr -> HighlightM a
-- throwFileErr = throwHighlightErr . HighlightFileErr

-- throwFileAlreadyInUseErr :: Monad m => FilePath -> HighlightM a
-- throwFileAlreadyInUseErr = throwFileErr . FileAlreadyInUseErr

-- throwFileDoesNotExistErr :: Monad m => FilePath -> HighlightM a
-- throwFileDoesNotExistErr = throwFileErr . FileDoesNotExistErr

-- throwFilePermissionErr :: Monad m => FilePath -> HighlightM a
-- throwFilePermissionErr = throwFileErr . FilePermissionErr

-----------
-- Pipes --
-----------

data InputSource
  = InputSourceStdin
  | InputSourceSingleFile FilePath
  | InputSourceMultiFile FilePath

-- getInput
--   :: HighlightM (Producer (InputSource, FreeT (Producer ByteString m) m x) m x)
-- getInput = do
--   inputFileNames <- getInputFileNames
--   recursive <- getRecursive
--   case (inputFileNames, recursive) of
--     ([], _) -> undefined -- from stdin
--     ([singleFile], NotRecursive) ->
--         producerForFile SingleFileNotRecursive singleFile
--     ([singleFile], Recursive) ->
--         producerForFile SingleFileRecursive singleFile
--     (multiFiles, NotRecursive) -> undefined
--     (multiFiles, Recursive) -> undefined

data MultiFileType
  = MultiFileNotRecursive
  | MultiFileRecursive
  | SingleFileNotRecursive
  | SingleFileRecursive

type Lala =
  Producer
    ( Either
        (IOException, FilePath)
        ( InputSource
        , FreeT
            (Producer ByteString HighlightMWithIO)
            HighlightMWithIO
            ()
        )
    )
    HighlightMWithIO
    ()

data WhereDidFileComeFrom
  = FileSpecifiedByUser FilePath
  | FileFoundRecursively FilePath

data OriginalInputSource
  = OriginalInputSourceStdin
  | OriginalInputSourceUserSpecifiedSingleFileNotRecursive WhereDidFileComeFrom
  | OriginalInputSourceUserSpecifiedMultiFile
      (Producer WhereDidFileComeFrom HighlightMWithIO ())

createOriginalInputSource :: HighlightM OriginalInputSource
createOriginalInputSource = do
  inputFilenames <- getInputFilenames
  recursive <- getRecursive
  case (inputFilenames, recursive) of
    ([], _) -> pure OriginalInputSourceStdin
    ([InputFilename singleFile], NotRecursive) ->
      pure $ OriginalInputSourceUserSpecifiedSingleFileNotRecursive $
        FileSpecifiedByUser singleFile
    ([InputFilename singleFile], Recursive) ->
      undefined
      -- createMultiFile [FileSpecifiedByUser singleFile]
    (multiFiles, NotRecursive) ->
      pure .  OriginalInputSourceUserSpecifiedMultiFile .  each $
        fmap (FileSpecifiedByUser . unInputFilename) multiFiles
    (multiFiles, Recursive) -> undefined
      -- createMultiFile (fmap FileSpecifiedByUser multiFiles) Recursive

producerForFile
  :: MultiFileType
  -> FilePath
  -> Lala
producerForFile SingleFileNotRecursive filePath = do
  eitherHandle <- openFilePathForReading filePath
  case eitherHandle of
    Right handle -> do
      let linesFreeTProducer = fromHandle handle ^. Pipes.ByteString.lines
      yield $ Right (InputSourceSingleFile filePath, linesFreeTProducer)
    Left ioerr -> yield $ Left (ioerr, filePath)
producerForFile SingleFileRecursive filePath = do
  eitherHandle <- openFilePathForReading filePath
  case eitherHandle of
    Right handle -> do
      let linesFreeTProducer = fromHandle handle ^. Pipes.ByteString.lines
      yield $ Right (InputSourceSingleFile filePath, linesFreeTProducer)
    Left ioerr -> do
      -- filePathProducer <- enumerate $ childOf filePath
      undefined


openFilePathForReading :: MonadIO m => FilePath -> m (Either IOException Handle)
openFilePathForReading filePath =
  liftIO . try $ openBinaryFile filePath ReadMode

-- throwIOError :: MonadIO m => IOException -> m a
-- throwIOError = liftIO . ioError
