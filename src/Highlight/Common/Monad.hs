{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Highlight.Common.Monad
  ( module Highlight.Common.Monad
  , module Highlight.Common.Monad.Input
  , module Highlight.Common.Monad.Output
  ) where

import Prelude ()
import Prelude.Compat

import Control.Lens (view)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Pipes (Producer, next, yield)
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        compileRegexWith)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad.Input
import Highlight.Common.Monad.Output
import Highlight.Common.Options
       (HasIgnoreCase(ignoreCaseLens),
        HasInputFilenames(inputFilenamesLens), HasRecursive(recursiveLens),
        HasRawRegex(rawRegexLens), IgnoreCase(DoNotIgnoreCase, IgnoreCase),
        InputFilename, RawRegex(RawRegex), Recursive)

--------------------------------
-- The Common Highlight Monad --
--------------------------------

newtype CommonHighlightM r s e a = CommonHighlightM
  { unCommonHighlightM :: ReaderT r (StateT s (ExceptT e IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError e
             , MonadIO
             , MonadReader r
             , MonadState s
             )

runCommonHighlightM :: r -> s -> CommonHighlightM r s e a -> IO (Either e a)
runCommonHighlightM r s =
  runExceptT .
    flip evalStateT s .
    flip runReaderT r .
    unCommonHighlightM

getIgnoreCaseM :: (HasIgnoreCase r, MonadReader r m) => m IgnoreCase
getIgnoreCaseM  = view ignoreCaseLens

getRecursiveM :: (HasRecursive r, MonadReader r m) => m Recursive
getRecursiveM = view recursiveLens

getRawRegexM :: (HasRawRegex r, MonadReader r m) => m RawRegex
getRawRegexM = view rawRegexLens

getInputFilenamesM
  :: (HasInputFilenames r, MonadReader r m) => m [InputFilename]
getInputFilenamesM = view inputFilenamesLens

------------------
-- Throw Errors --
------------------

throwHighlightErr :: HighlightErr -> CommonHighlightM r s HighlightErr a
throwHighlightErr = throwError

throwRegexCompileErr :: RawRegex -> CommonHighlightM r s HighlightErr a
throwRegexCompileErr = throwHighlightErr . HighlightRegexCompileErr

-----------------------
-- Filename Handling --
-----------------------

computeFilenameHandlingFromFiles
  :: forall a m r.
     Monad m
  => Producer (FileOrigin, a) m r
  -> m (FilenameHandlingFromFiles, Producer (FileOrigin, a) m r)
computeFilenameHandlingFromFiles producer = do
  eitherFirstFile <- next producer
  case eitherFirstFile of
    Left ret ->
      return (NoFilename, return ret)
    Right ((fileOrigin1, a1), producer2) ->
      case fileOrigin1 of
        Stdin -> error "Not currenty handling stdin..."
        FileSpecifiedByUser _ -> do
          eitherSecondFile <- next producer2
          case eitherSecondFile of
            Left ret2 ->
              return (NoFilename, yield (fileOrigin1, a1) *> return ret2)
            Right ((fileOrigin2, a2), producer3) ->
              return
                ( PrintFilename
                , yield (fileOrigin1, a1) *> yield (fileOrigin2, a2) *> producer3
                )
        FileFoundRecursively _ ->
          return (PrintFilename, yield (fileOrigin1, a1) *> producer2)

-----------
-- Regex --
-----------

compileHighlightRegexWithErr
  :: (HasIgnoreCase r, HasRawRegex r)
  => CommonHighlightM r s HighlightErr RE
compileHighlightRegexWithErr = do
  ignoreCase <- getIgnoreCaseM
  rawRegex <- getRawRegexM
  case compileHighlightRegex ignoreCase rawRegex of
    Just re -> return re
    Nothing -> throwRegexCompileErr rawRegex

compileHighlightRegex :: IgnoreCase -> RawRegex -> Maybe RE
compileHighlightRegex ignoreCase (RawRegex rawRegex) =
  let simpleREOptions =
        case ignoreCase of
          IgnoreCase -> MultilineInsensitive
          DoNotIgnoreCase -> MultilineSensitive
  in compileRegexWith simpleREOptions rawRegex
