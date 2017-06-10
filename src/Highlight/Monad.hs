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
import Data.ByteString (ByteString, hGetLine)
import Data.DirStream (childOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup ((<>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import qualified Filesystem.Path.CurrentOS as Path
import Pipes
       (Effect, Pipe, Producer, (>->), await, each, enumerate, for, next,
        runEffect, yield)
import qualified Pipes.ByteString
import Pipes.Group (concats)
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude as Pipes
import Pipes.Safe (SafeT, runSafeT)
import System.IO (Handle, openBinaryFile, stdin)

import Highlight.Error (HighlightErr(..))
import Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames, DoNotColorGrepFileNames),
        IgnoreCase, InputFilename(unInputFilename), Options(..), RawRegex,
        Recursive(Recursive))
import Highlight.Util
       (combineApplicatives, convertStringToRawByteString,
        openFilePathForReading, unsafeConvertStringToRawByteString)

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
        (Producer ByteString m a)
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
      let filenameHandling = computeFilenameHandlingFromStdin colorGrepFilenames
      pure . InputDataStdin filenameHandling $ fromHandleLines stdin
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
      let linesProducer = fromHandleLines handle
      pure $ yield (whereDid, Right linesProducer)
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

fromHandleLines :: forall m. MonadIO m => Handle -> Producer ByteString m ()
fromHandleLines handle = go
  where
    go :: Producer ByteString m ()
    go = do
      eitherLine <- liftIO . try $ hGetLine handle
      case eitherLine of
        Left (ioerr :: IOException) -> return ()
        Right line -> yield line *> go

data InputData m a
  = InputDataStdin
      FilenameHandlingFromStdin
      (Producer ByteString m a)
  | InputDataFile
      FilenameHandlingFromFiles
      (Lala m a)

handleInputData
  :: (FilenameHandlingFromStdin -> ByteString -> ByteString)
  -> (FilenameHandlingFromFiles -> ByteString -> ByteString -> NonEmpty ByteString)
  -> InputData HighlightMWithIO ()
  -> HighlightM ()
handleInputData f _ (InputDataStdin filenameHandling producer) =
  handleInputDataStdin f filenameHandling producer
handleInputData _ f (InputDataFile filenameHandling lala) = do
  handleInputDataFile f filenameHandling lala

handleInputDataFile
  :: (FilenameHandlingFromFiles -> ByteString -> ByteString -> NonEmpty ByteString)
  -> FilenameHandlingFromFiles
  -> Lala HighlightMWithIO ()
  -> HighlightM ()
handleInputDataFile f filenameHandling lala = do
  -- TODO: I think I can probably use the 'for' function to loop through lala?
  unHighlightMWithIO . liftIO $ print filenameHandling
  unHighlightMWithIO . runEffect $ for lala g
  where
    g
      :: ( WhereDidFileComeFrom
         , Either
            (IOException, Maybe IOException)
            (Producer ByteString HighlightMWithIO ())
         )
      -> Effect HighlightMWithIO ()
    g (whereDid, Left (ioerr, maybeioerr)) = undefined
    g (whereDid, Right producer) = do
      let filePath = getFilePathFromWhereDid whereDid
      -- TODO: Need to free this filePath
      byteStringFilePath <- liftIO $ convertStringToRawByteString filePath
      producer >-> bababa byteStringFilePath >-> Pipes.ByteString.stdout
      where
        bababa :: ByteString -> Pipe ByteString ByteString HighlightMWithIO ()
        bababa filePath = do
          inputLine <- await
          let outputLines = f filenameHandling filePath inputLine
          each outputLines
          yield "\n"
          bababa filePath

handleInputDataStdin
  :: (FilenameHandlingFromStdin -> ByteString -> ByteString)
  -> FilenameHandlingFromStdin
  -> Producer ByteString HighlightMWithIO ()
  -> HighlightM ()
handleInputDataStdin f filenameHandling producer = do
  unHighlightMWithIO . liftIO $ print filenameHandling
  unHighlightMWithIO . runEffect $
    producer >-> addNewline (f filenameHandling) >-> Pipes.ByteString.stdout

addNewline
  :: forall m. Monad m
  => (ByteString -> ByteString) -> Pipe ByteString ByteString m ()
addNewline func = go
  where
    go :: Pipe ByteString ByteString m ()
    go = do
      inputLine <- await
      yield $ func inputLine
      yield "\n"
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
