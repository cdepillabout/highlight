module Highlight.Util where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import Data.Semigroup (Semigroup, (<>))
import Foreign.C (newCStringLen)
import System.IO (Handle, IOMode(ReadMode), openBinaryFile)
import System.IO.Unsafe (unsafePerformIO)

-- | Convert a String to a ByteString with the encoding for the current locale.
convertStringToRawByteString :: MonadIO m => String -> m ByteString
convertStringToRawByteString str = liftIO $ do
  -- TODO: cStringLen should really be freed after we are finished using the
  -- resulting bytestring.
  cStringLen <- newCStringLen str
  unsafePackMallocCStringLen cStringLen

unsafeConvertStringToRawByteString :: String -> ByteString
unsafeConvertStringToRawByteString =
  unsafePerformIO . convertStringToRawByteString

-- | TODO: Do I need to register this Handle to close?  Or does it do it
-- automatically on it's finalizer?
openFilePathForReading :: MonadIO m => FilePath -> m (Either IOException Handle)
openFilePathForReading filePath =
  liftIO . try $ openBinaryFile filePath ReadMode

combineApplicatives :: (Applicative f, Semigroup a) => f a -> f a -> f a
combineApplicatives action1 action2 =
  (<>) <$> action1 <*> action2
