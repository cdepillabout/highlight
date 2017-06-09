{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Monad where

import Control.Lens ((^.))
import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Free (FreeT)
import Data.ByteString (ByteString)
import Data.DirStream (childOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (Semigroup, (<>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import qualified Filesystem.Path.CurrentOS as Path
import Pipes
       (Pipe, Producer, (>->), await, each, enumerate, for, next,
        runEffect, yield)
import Pipes.ByteString (fromHandle, stdin)
import qualified Pipes.ByteString
import Pipes.Group (concats)
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude as Pipes
import Pipes.Safe (SafeT, runSafeT)
import System.IO (Handle, IOMode(ReadMode), openBinaryFile)

import Highlight.Error (HighlightErr(..))
import Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        IgnoreCase, InputFilename(unInputFilename), Options(..), RawRegex,
        Recursive(Recursive))
import Highlight.Util (unsafeConvertStringToRawByteString)


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

-----------
-- Pipes --
-----------

type Lala m a =
  Producer
    ( WhereDidFileComeFrom
    , Either
        (IOException, Maybe IOException)
        (FreeT (Producer ByteString HighlightMWithIO) m a)
    )
    m
    a

data WhereDidFileComeFrom
  = FileSpecifiedByUser FilePath
  | FileFoundRecursively FilePath

getFilePathFromWhereDid :: WhereDidFileComeFrom -> FilePath
getFilePathFromWhereDid (FileSpecifiedByUser fp) = fp
getFilePathFromWhereDid (FileFoundRecursively fp) = fp

createInputData :: HighlightM (InputData HighlightMWithIO ())
createInputData = do
  inputFilenames <-
    fmap (FileSpecifiedByUser . unInputFilename) <$> getInputFilenames
  recursive <- getRecursive
  colorGrepFilenames <- getColorGrepFilenames
  case inputFilenames of
    [] -> do
      -- TODO: This doesn't work when the input file has lines over 32kb.
      -- Need to rewrite 'stdin' and 'lines'.
      let filenameHandling = computeFilenameHandlingFromStdin colorGrepFilenames
      pure . InputDataStdin filenameHandling $ stdin ^. Pipes.ByteString.lines
    (file1:files) -> do
      lalas <-
        unHighlightMWithIO $
          traverse
            (producerForSingleFilePossiblyRecursive recursive)
            (file1 :| files)
      let lala = foldl1 combineApplicatives lalas
      filenameHandling <-
        unHighlightMWithIO $ computeFilenameHandlingFromFiles lala
      pure $ InputDataFile filenameHandling lala

combineApplicatives :: (Applicative f, Semigroup a) => f a -> f a -> f a
combineApplicatives action1 action2 =
  (<>) <$> action1 <*> action2

-- | TODO: This is a complicated function.
producerForSingleFilePossiblyRecursive
  :: Recursive
  -> WhereDidFileComeFrom
  -> HighlightMWithIO (Lala HighlightMWithIO ())
producerForSingleFilePossiblyRecursive recursive whereDid = do
  let filePath = getFilePathFromWhereDid whereDid
  eitherHandle <- openFilePathForReading filePath
  case eitherHandle of
    Right handle -> do
      -- TODO: This doesn't work when the input file has lines over 32kb.
      -- Need to rewrite 'fromHandle' and 'lines'.
      let linesFreeTProducer = fromHandle handle ^. Pipes.ByteString.lines
      pure $ yield (whereDid, Right linesFreeTProducer)
    Left fileIOErr ->
      if recursive == Recursive
        then do
          let listT = childOf $ decodeString filePath
              producer =
                enumerate listT :: Producer Path.FilePath (SafeT IO) ()
              lIO = runSafeT $ toListM producer :: IO [Path.FilePath]
          eitherFileList <- liftIO (try lIO)
          case eitherFileList of
            Left dirIOErr ->
              pure $ yield (whereDid, Left (fileIOErr, Just dirIOErr))
            Right fileList -> do
              let whereDids =
                    fmap (FileFoundRecursively . encodeString) fileList
              lalas <-
                traverse
                  (producerForSingleFilePossiblyRecursive recursive)
                  whereDids
              pure $ for (each lalas) id
        else
          pure $ yield (whereDid, Left (fileIOErr, Nothing))

data InputData m a
  = InputDataStdin
      FilenameHandlingFromStdin
      (FreeT (Producer ByteString m) m a)
  | InputDataFile
      FilenameHandlingFromFiles
      (Lala m a)

handleInputData :: InputData HighlightMWithIO () -> HighlightM ()
handleInputData (InputDataStdin filenameHandling freeT) = do
  unHighlightMWithIO . liftIO $ print filenameHandling
  unHighlightMWithIO . runEffect $
    concats freeT >-> f 0 >-> Pipes.print
  where
    f :: Int -> Pipe ByteString ByteString HighlightMWithIO ()
    f int = do
      inputLine <- await
      let outputLine =
            "line " <>
            unsafeConvertStringToRawByteString (show int) <>
            ": " <>
            inputLine
      yield outputLine
      f (int + 1)
handleInputData (InputDataFile filenameHandling lala) = do
  unHighlightMWithIO . liftIO $ print filenameHandling
  unHighlightMWithIO . runEffect $
    lala >-> f >-> Pipes.print
  where
    f :: Pipe (WhereDidFileComeFrom, b) String HighlightMWithIO ()
    f = do
      (whereDid, _) <- await
      let filePath = getFilePathFromWhereDid whereDid
      yield filePath
      f

-- | TODO: Do I need to register this Handle to close?  Or does it do it
-- automatically on it's finalizer?
openFilePathForReading :: MonadIO m => FilePath -> m (Either IOException Handle)
openFilePathForReading filePath =
  liftIO . try $ openBinaryFile filePath ReadMode

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

data FilenameHandlingFromFiles
  = FromFilesNoFilename
  | FromFilesPrintFilename
  deriving (Eq, Read, Show)

computeFilenameHandlingFromFiles
  :: forall a m r.
     Monad m
  => Producer (WhereDidFileComeFrom, a) m r
  -> m FilenameHandlingFromFiles
computeFilenameHandlingFromFiles producer = do
  eitherFirstFile <- next producer
  case eitherFirstFile of
    Left _ -> pure FromFilesNoFilename
    Right ((whereDid, _), producer2) ->
      case whereDid of
        FileSpecifiedByUser _ -> do
          eitherSecondFile <- next producer2
          case eitherSecondFile of
            Left _ -> pure FromFilesNoFilename
            Right (_, _) -> pure FromFilesPrintFilename
        FileFoundRecursively _ -> pure FromFilesPrintFilename
