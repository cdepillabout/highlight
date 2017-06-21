{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Highlight.Common.Pipes where

import Prelude ()
import Prelude.Compat

import Control.Exception (throwIO, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString, hGetLine, hPutStr)
import Foreign.C.Error (Errno(Errno), ePIPE)
import GHC.IO.Exception
       (IOException(IOError), IOErrorType(ResourceVanished), ioe_errno,
        ioe_type)
import Pipes (Consumer', Producer, await, each, yield)
import qualified Pipes.Prelude as Pipes
import Pipes.Safe (MonadSafe, bracket)
import System.FilePath ((</>))
import System.IO (Handle, stderr, stdin)
#ifdef mingw32_HOST_OS
#else
import System.Posix.Directory
       (DirStream, closeDirStream, openDirStream, readDirStream)
#endif

import Highlight.Common.Util
       (closeHandleIfEOFOrThrow, openFilePathForReading)

-- | Read input from a 'Handle', split it into lines, and return each of those
-- lines as a 'ByteString' in a 'Producer'.
--
-- This function will close the 'Handle' if the end of the file is reached.
-- However, if an error was thrown while reading input from the 'Handle', the
-- 'Handle' is not closed.
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

stdinLines :: forall m. MonadIO m => Producer ByteString m ()
stdinLines = fromHandleLines stdin
{-# INLINABLE stdinLines #-}

fromFileLines
  :: forall m n.
     (MonadIO m, MonadIO n)
  => FilePath
  -> m (Either IOException (Producer ByteString n ()))
fromFileLines filePath = do
  eitherHandle <- openFilePathForReading filePath
  case eitherHandle of
    Left ioerr -> return $ Left ioerr
    Right handle -> return . Right $ fromHandleLines handle

-- | Number each value in a 'Producer'.
--
-- >>> import Pipes.Prelude (toList)
-- >>> let producer = each [("dog", 3.3), ("bird", 10.1), ("cat", 25.5)]
-- >>> toList $ numberedProducer producer
-- [(0,"dog",3.3),(1,"bird",10.1),(2,"cat",25.5)]
numberedProducer
  :: forall a b m.  Monad m => Producer (a, b) m () -> Producer (Int, a, b) m ()
numberedProducer = Pipes.zipWith (\int (a, b) -> (int, a, b)) $ each [0..]
{-# INLINABLE numberedProducer #-}

-- | Output 'ByteString's to 'stderr'.
--
-- If an 'ePIPE' error is thrown, then just 'return' @()@.  If any other error
-- is thrown, rethrow the error.
stderrConsumer :: forall m. MonadIO m => Consumer' ByteString m ()
stderrConsumer = go
  where
    go :: Consumer' ByteString m ()
    go = do
      bs <- await
      x  <- liftIO $ try (hPutStr stderr bs)
      case x of
        Left (IOError { ioe_type = ResourceVanished, ioe_errno = Just ioe })
          | Errno ioe == ePIPE -> return ()
        Left  e  -> liftIO $ throwIO e
        Right () -> go
{-# INLINABLE stderrConsumer #-}


-- | Select all immediate children of the given directory, ignoring @\".\"@ and
-- @\"..\"@.
--
-- Throws an 'IOException' if the directory is not readable or (on Windows) if
-- the directory is actually a reparse point.
childOf :: MonadSafe m => FilePath -> Producer FilePath m ()
childOf path = do
#ifdef mingw32_HOST_OS
  -- reparse <- liftIO $ fmap reparsePoint $ Win32.getFileAttributes path
  -- when (canRead && not reparse) $
  --     bracket
  --         (liftIO $ Win32.findFirstFile (F.encodeString (path </> "*")))
  --         (\(h, _) -> liftIO $ Win32.findClose h)
  --         $ \(h, fdat) -> do
  --             let loop = do
  --                     file' <- liftIO $ Win32.getFindDataFileName fdat
  --                     let file = F.decodeString file'
  --                     when (file' /= "." && file' /= "..") $
  --                         yield (path </> file)
  --                     more  <- liftIO $ Win32.findNextFile h fdat
  --                     when more loop
  --             loop
	error "Not implemented yet on Windows. Please send PR."
#else
  bracket (liftIO $ openDirStream path) (liftIO . closeDirStream) go
  where
    go :: MonadIO m => DirStream -> Producer FilePath m ()
    go dirp = loop
      where
        loop :: MonadIO m => Producer FilePath m ()
        loop = do
          file <- liftIO $ readDirStream dirp
          case file of
            [] -> return ()
            _  -> do
              when (file /= "." && file /= "..") . yield $ path </> file
              loop
#endif
{-# INLINABLE childOf #-}
