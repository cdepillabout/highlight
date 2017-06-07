{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Highlight.Monad where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Except (ExceptT, runExceptT, throwError)

import Highlight.Error (HighlightErr)
import Highlight.Options
       (ColorGrepFilenames, IgnoreCase, InputFilename, Options(..),
        RawRegex, Recursive)

newtype HighlightT m a = HighlightT
  { unHighlightM :: ReaderT Options (ExceptT HighlightErr m) a
  } deriving (Functor, Applicative, Monad, MonadIO)

type HighlightM = HighlightT IO

runHighlightT :: Options -> HighlightT m a -> m (Either HighlightErr a)
runHighlightT opts = runExceptT . flip runReaderT opts . unHighlightM

getOptions :: Monad m => HighlightT m Options
getOptions = HighlightT ask

getIgnoreCase :: Monad m => HighlightT m IgnoreCase
getIgnoreCase  = optionsIgnoreCase <$> getOptions

getRecursive :: Monad m => HighlightT m Recursive
getRecursive = optionsRecursive <$> getOptions

getColorGrepFilenames :: Monad m => HighlightT m ColorGrepFilenames
getColorGrepFilenames = optionsColorGrepFilenames <$> getOptions

getRawRegex :: Monad m => HighlightT m RawRegex
getRawRegex = optionsRawRegex <$> getOptions

getInputFilenames :: Monad m => HighlightT m [InputFilename]
getInputFilenames = optionsInputFilenames <$> getOptions

throwHighlightErr :: Monad m => HighlightErr -> HighlightT m a
throwHighlightErr = HighlightT . throwError

throwRegexCompileErr :: Monad m => RawRegex -> HighlightT m a
throwRegexCompileErr = throwHighlightErr . HighlightRegexCompileErr

throwFileErr :: Monad m => FileErr -> HighlightT m a
throwFileErr = throwHighlightErr . HighlightFileErr

throwFileAlreadyInUseErr :: Monad m => FilePath -> HighlightT m a
throwFileAlreadyInUseErr = throwFileErr . FileAlreadyInUseErr

throwFileDoesNotExistErr :: Monad m => FilePath -> HighlightT m a
throwFileDoesNotExistErr = throwFileErr . FileDoesNotExistErr

throwFilePermissionErr :: Monad m => FilePath -> HighlightT m a
throwFilePermissionErr = throwFileErr . FilePermissionErr
