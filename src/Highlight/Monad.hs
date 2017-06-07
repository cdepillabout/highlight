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
import Data.Foldable (asum)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import qualified Filesystem.Path.CurrentOS as Path
import Pipes
       (Pipe, Producer, (>->), await, each, enumerate, for, runEffect,
        yield)
import Pipes.ByteString (fromHandle, stdin)
import qualified Pipes.ByteString
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude
import Pipes.Safe (SafeT, runSafeT)
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
    ( WhereDidFileComeFrom
    , Either
        IOException
        ( FreeT
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

getFilePathFromWhereDid :: WhereDidFileComeFrom -> FilePath
getFilePathFromWhereDid (FileSpecifiedByUser fp) = fp
getFilePathFromWhereDid (FileFoundRecursively fp) = fp

-- | TODO: This can't use OriginalInputSource, so we actually need to return
-- 'InputData'.
createInputData :: HighlightM InputData
createInputData = do
  inputFilenames <- getInputFilenames
  recursive <- getRecursive
  case (inputFilenames, recursive) of
    ([], _) ->
      pure . InputDataStdin $ stdin ^. Pipes.ByteString.lines
    ([InputFilename singleFile], NotRecursive) -> do
      eitherProducer <- unHighlightMWithIO $ producerForSingleFile singleFile
      pure $ InputDataSingleFile singleFile eitherProducer
    ([InputFilename singleFile], Recursive) -> do
      lala <-
        unHighlightMWithIO $
          producerForSingleFilePossiblyRecursive $
            FileSpecifiedByUser singleFile
      pure $ InputDataMultiFile lala
    (multiFiles, NotRecursive) ->
      undefined
      -- pure .  OriginalInputSourceMultiFile .  each $
      --   fmap (FileSpecifiedByUser . unInputFilename) multiFiles
    (multiFiles, Recursive) -> do
      undefined
      -- producer <-
      --   unHighlightMWithIO $
      --     createMultiFile $
      --       fmap (FileSpecifiedByUser . unInputFilename) multiFiles
      -- pure $ OriginalInputSourceMultiFile producer

producerForSingleFilePossiblyRecursive
  :: WhereDidFileComeFrom
  -> HighlightMWithIO Lala
producerForSingleFilePossiblyRecursive whereDid = do
  let filePath = getFilePathFromWhereDid whereDid
  eitherHandle <- openFilePathForReading filePath
  case eitherHandle of
    Right handle -> do
      let linesFreeTProducer = fromHandle handle ^. Pipes.ByteString.lines
      pure $ yield (whereDid, Right linesFreeTProducer)
    Left ioerr -> do
      -- let listT =
      --       asum $ fmap (childOf . decodeString . getFilePathFromWhereDid) whereDids
      --     producer = enumerate listT :: Producer Path.FilePath (SafeT IO) ()
      let listT = childOf $ decodeString filePath
          producer = enumerate listT :: Producer Path.FilePath (SafeT IO) ()
          lIO = runSafeT $ toListM producer :: IO [Path.FilePath]
      fileList <- liftIO lIO
      let whereDids = fmap (FileFoundRecursively . encodeString) fileList
      lalas <- traverse producerForSingleFilePossiblyRecursive whereDids
      pure $ for (each lalas) id

-- createMultiFile
--   :: [WhereDidFileComeFrom]
--   -> HighlightMWithIO (Producer WhereDidFileComeFrom HighlightMWithIO ())
-- createMultiFile whereDids = do
--   let listT =
--         asum $ fmap (childOf . decodeString . getFilePathFromWhereDid) whereDids
--       producer = enumerate listT :: Producer Path.FilePath HighlightMWithIO ()
--   undefined


data InputData
  = InputDataStdin
      (FreeT (Producer ByteString HighlightMWithIO) HighlightMWithIO ())
  | InputDataSingleFile
      FilePath
      ( Either
          IOException
          (FreeT (Producer ByteString HighlightMWithIO) HighlightMWithIO ())
      )
  | InputDataMultiFile Lala

producerForSingleFile
  :: FilePath
  -> HighlightMWithIO
      ( Either
          IOException
          (FreeT (Producer ByteString HighlightMWithIO) HighlightMWithIO ())
      )
producerForSingleFile filePath = do
  eitherHandle <- openFilePathForReading filePath
  case eitherHandle of
    Right handle -> do
      let linesFreeTProducer = fromHandle handle ^. Pipes.ByteString.lines
      pure $ Right linesFreeTProducer
    Left ioerr -> pure $ Left ioerr

-- producerForFile
--   :: MultiFileType
--   -> FilePath
--   -> Lala
-- producerForFile SingleFileNotRecursive filePath = do
--   eitherHandle <- openFilePathForReading filePath
--   case eitherHandle of
--     Right handle -> do
--       let linesFreeTProducer = fromHandle handle ^. Pipes.ByteString.lines
--       yield $ Right (InputSourceSingleFile filePath, linesFreeTProducer)
--     Left ioerr -> yield $ Left (ioerr, filePath)
-- producerForFile SingleFileRecursive filePath = do
--   eitherHandle <- openFilePathForReading filePath
--   case eitherHandle of
--     Right handle -> do
--       let linesFreeTProducer = fromHandle handle ^. Pipes.ByteString.lines
--       yield $ Right (InputSourceSingleFile filePath, linesFreeTProducer)
--     Left ioerr -> do
--       -- filePathProducer <- enumerate $ childOf filePath
--       undefined

handleInputData :: InputData -> HighlightM ()
handleInputData (InputDataMultiFile lala) =
  unHighlightMWithIO $ runEffect $ lala >-> f >-> Pipes.Prelude.print
  where
    f :: Pipe (WhereDidFileComeFrom, b) String HighlightMWithIO ()
    f = do
      (whereDid, _) <- await
      let filePath = getFilePathFromWhereDid whereDid
      yield filePath
      f

openFilePathForReading :: MonadIO m => FilePath -> m (Either IOException Handle)
openFilePathForReading filePath =
  liftIO . try $ openBinaryFile filePath ReadMode

-- throwIOError :: MonadIO m => IOException -> m a
-- throwIOError = liftIO . ioError
