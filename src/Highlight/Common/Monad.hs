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
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        compileRegexWith)

import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Monad.Input
       (FilenameHandlingFromFiles(NoFilename, PrintFilename), InputData,
        createInputData)
import Highlight.Common.Monad.Output
       (Output(OutputStderr, OutputStdout), handleInputData,
        outputConsumer)
import Highlight.Common.Options
       (HasIgnoreCase(ignoreCaseLens),
        HasInputFilenames(inputFilenamesLens), HasRecursive(recursiveLens),
        HasRawRegex(rawRegexLens), IgnoreCase(DoNotIgnoreCase, IgnoreCase),
        InputFilename, RawRegex(RawRegex), Recursive)

--------------------------------
-- The Common Highlight Monad --
--------------------------------

-- | This is the common monad for both @highlight@ and @hrep@.  It has been
-- kept polymorphic here so it can be easily specialized by @highlight@ and
-- @hrep@.
--
-- @r@ is the options or config type.  @s@ is the state.  @e@ is the error.
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

-- | Given an @r@ and @s@, run 'CommonHighlightM'.
runCommonHighlightM :: r -> s -> CommonHighlightM r s e a -> IO (Either e a)
runCommonHighlightM r s =
  runExceptT .
    flip evalStateT s .
    flip runReaderT r .
    unCommonHighlightM

-- | Get the 'IgnoreCase' option.
getIgnoreCaseM :: (HasIgnoreCase r, MonadReader r m) => m IgnoreCase
getIgnoreCaseM  = view ignoreCaseLens

-- | Get the 'Recursive' option.
getRecursiveM :: (HasRecursive r, MonadReader r m) => m Recursive
getRecursiveM = view recursiveLens

-- | Get the 'RawRegex' option.
getRawRegexM :: (HasRawRegex r, MonadReader r m) => m RawRegex
getRawRegexM = view rawRegexLens

-- | Get a list of the 'InputFilename'.
getInputFilenamesM
  :: (HasInputFilenames r, MonadReader r m) => m [InputFilename]
getInputFilenamesM = view inputFilenamesLens

------------------
-- Throw Errors --
------------------

-- | Throw a 'HighlightErr'.
throwHighlightErr :: HighlightErr -> CommonHighlightM r s HighlightErr a
throwHighlightErr = throwError

-- | Throw a 'HighlightRegexCompileErr'.
throwRegexCompileErr :: RawRegex -> CommonHighlightM r s HighlightErr a
throwRegexCompileErr = throwHighlightErr . HighlightRegexCompileErr

-----------
-- Regex --
-----------

-- | Call 'compileHighlightRegex'.  Throw a 'HighlightErr' if the regex cannot
-- be compiled.
compileHighlightRegexWithErr
  :: (HasIgnoreCase r, HasRawRegex r)
  => CommonHighlightM r s HighlightErr RE
compileHighlightRegexWithErr = do
  ignoreCase <- getIgnoreCaseM
  rawRegex <- getRawRegexM
  case compileHighlightRegex ignoreCase rawRegex of
    Just re -> return re
    Nothing -> throwRegexCompileErr rawRegex

-- | Try compiling a 'RawRegex' into a 'RE'.
--
-- Setup for examples:
--
-- >>> import Data.Maybe (isJust)
--
-- Return 'Just' for a proper regex:
--
-- >>> isJust $ compileHighlightRegex IgnoreCase (RawRegex "good regex")
-- True
--
-- Return 'Nothing' for an improper regex:
--
-- >>> isJust $ compileHighlightRegex IgnoreCase (RawRegex "bad regex (")
-- False
compileHighlightRegex :: IgnoreCase -> RawRegex -> Maybe RE
compileHighlightRegex ignoreCase (RawRegex rawRegex) =
  let simpleREOptions =
        case ignoreCase of
          IgnoreCase -> MultilineInsensitive
          DoNotIgnoreCase -> MultilineSensitive
  in compileRegexWith simpleREOptions rawRegex
