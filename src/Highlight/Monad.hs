{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
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
       (Pipe, Producer, (>->), await, each, enumerate, for, runEffect,
        yield)
import Pipes.ByteString (fromHandle, stdin)
import qualified Pipes.ByteString
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude
import Pipes.Safe (SafeT, runSafeT)
import System.IO (Handle, IOMode(ReadMode), openBinaryFile)

import Highlight.Error (HighlightErr(..))
import Highlight.Options
       (ColorGrepFilenames, IgnoreCase,
        InputFilename(unInputFilename), Options(..),
        RawRegex, Recursive(Recursive))


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


-- | TODO: Change this to 'Lala m a' where @m@ will be 'HighlightMWithIO' and
-- @a@ will be ().
type Lala =
  Producer
    ( WhereDidFileComeFrom
    , Either
        (IOException, Maybe IOException)
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

createInputData :: HighlightM InputData
createInputData = do
  inputFilenames <-
    fmap (FileSpecifiedByUser . unInputFilename) <$> getInputFilenames
  recursive <- getRecursive
  case inputFilenames of
    [] ->
      pure . InputDataStdin $ stdin ^. Pipes.ByteString.lines
    [singleFile] -> do
      lala <-
        unHighlightMWithIO $
          producerForSingleFilePossiblyRecursive recursive singleFile
      pure $ InputDataFile lala
    (file1:files) -> do
      lalas <-
        unHighlightMWithIO $
          traverse
            (producerForSingleFilePossiblyRecursive recursive)
            (file1 :| files)
      pure . InputDataFile $ foldl1 combineApplicatives lalas

combineApplicatives :: (Applicative f, Semigroup a) => f a -> f a -> f a
combineApplicatives action1 action2 =
  (<>) <$> action1 <*> action2

-- | TODO: This is a complicated function.
producerForSingleFilePossiblyRecursive
  :: Recursive
  -> WhereDidFileComeFrom
  -> HighlightMWithIO Lala
producerForSingleFilePossiblyRecursive recursive whereDid = do
  let filePath = getFilePathFromWhereDid whereDid
  eitherHandle <- openFilePathForReading filePath
  case eitherHandle of
    Right handle -> do
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
  | InputDataFile Lala

handleInputData :: InputData -> HighlightM ()
handleInputData (InputDataFile lala) =
  unHighlightMWithIO $ runEffect $ lala >-> f >-> Pipes.Prelude.print
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
