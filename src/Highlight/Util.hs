module Highlight.Util where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import Data.Semigroup (Semigroup, (<>))
import Foreign.C (newCStringLen)
import System.IO (Handle, IOMode(ReadMode), hClose, hIsEOF, openBinaryFile)
import System.IO.Unsafe (unsafePerformIO)

-- | Convert a String to a ByteString with the encoding for the current locale.
convertStringToRawByteString :: MonadIO m => String -> m ByteString
convertStringToRawByteString str = liftIO $ do
  cStringLen <- newCStringLen str
  unsafePackMallocCStringLen cStringLen
{-# INLINABLE convertStringToRawByteString #-}

unsafeConvertStringToRawByteString :: String -> ByteString
unsafeConvertStringToRawByteString =
  unsafePerformIO . convertStringToRawByteString
{-# INLINABLE unsafeConvertStringToRawByteString #-}

openFilePathForReading :: MonadIO m => FilePath -> m (Either IOException Handle)
openFilePathForReading filePath =
  liftIO . try $ openBinaryFile filePath ReadMode
{-# INLINABLE openFilePathForReading #-}

combineApplicatives :: (Applicative f, Semigroup a) => f a -> f a -> f a
combineApplicatives action1 action2 =
  (<>) <$> action1 <*> action2
{-# INLINABLE combineApplicatives #-}

closeHandleIfEOFOrThrow :: MonadIO m => Handle -> IOException -> m ()
closeHandleIfEOFOrThrow handle ioerr = liftIO $ do
  isEOF <- hIsEOF handle
  if isEOF
    then hClose handle
    else ioError ioerr
{-# INLINABLE closeHandleIfEOFOrThrow #-}
