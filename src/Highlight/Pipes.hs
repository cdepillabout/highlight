{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Pipes where

import Control.Exception (throwIO, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString, hGetLine, hPutStr)
import Foreign.C.Error (Errno(Errno), ePIPE)
import GHC.IO.Exception
       (IOException(IOError), IOErrorType(ResourceVanished), ioe_errno,
        ioe_type)
import Pipes (Consumer', Producer, await, each, yield)
import qualified Pipes.Prelude as Pipes
import System.IO (Handle, stderr)

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
{-# INLINABLE fromHandleLines #-}

numberedProducer
  :: forall a b m.  Monad m => Producer (a, b) m () -> Producer (Int, a, b) m ()
numberedProducer = Pipes.zipWith (\int (a, b) -> (int, a, b)) $ each [0..]
{-# INLINABLE numberedProducer #-}

stderrConsumer :: forall m. MonadIO m => Consumer' ByteString m ()
stderrConsumer = go
  where
    go :: Consumer' ByteString m ()
    go = do
      bs <- await
      x  <- liftIO $ try (hPutStr stderr bs)
      case x of
        Left (IOError { ioe_type = ResourceVanished, ioe_errno = Just ioe })
          | Errno ioe == ePIPE -> pure ()
        Left  e  -> liftIO $ throwIO e
        Right () -> go
{-# INLINABLE stderrConsumer #-}
