{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Pipes where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString, hGetLine)
import Pipes
       (Effect, Pipe, Producer, (>->), await, each, enumerate, for, next,
        runEffect, yield)
import System.IO (Handle)

import Highlight.Util (closeHandleIfEOFOrThrow)

fromHandleLines :: forall m. MonadIO m => Handle -> Producer ByteString m ()
fromHandleLines handle = go
  where
    go :: Producer ByteString m ()
    go = do
      eitherLine <- liftIO . try $ hGetLine handle
      case eitherLine of
        Left ioerr -> closeHandleIfEOFOrThrow handle ioerr
        Right line -> yield line *> go
