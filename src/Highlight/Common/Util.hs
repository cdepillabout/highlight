module Highlight.Common.Util where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import Data.Semigroup (Semigroup, (<>))
import Foreign.C (newCStringLen)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (Handle, IOMode(ReadMode), hClose, hIsEOF, openBinaryFile)
import System.IO.Unsafe (unsafePerformIO)

-- | Convert a 'String' to a 'ByteString' with the encoding for the current
-- locale.
--
-- >>> convertStringToRawByteString "hello"
-- "hello"
convertStringToRawByteString :: MonadIO m => String -> m ByteString
convertStringToRawByteString str = liftIO $ do
  cStringLen <- newCStringLen str
  unsafePackMallocCStringLen cStringLen
{-# INLINABLE convertStringToRawByteString #-}

-- | Just like 'convertStringToRawByteString' but unsafe.
--
-- >>> unsafeConvertStringToRawByteString "bye"
-- "bye"
unsafeConvertStringToRawByteString :: String -> ByteString
unsafeConvertStringToRawByteString =
  unsafePerformIO . convertStringToRawByteString
{-# INLINABLE unsafeConvertStringToRawByteString #-}

-- | Open a 'FilePath' in 'ReadMode'.
--
-- On success, return a 'Right' 'Handle':
--
-- >>> openFilePathForReading "README.md"
-- Right {handle: README.md}
--
-- On error, return a 'Left' 'IOException':
--
-- >>> openFilePathForReading "thisfiledoesntexist"
-- Left thisfiledoesntexist: openBinaryFile: does not exist ...
openFilePathForReading :: MonadIO m => FilePath -> m (Either IOException Handle)
openFilePathForReading filePath =
  liftIO . try $ openBinaryFile filePath ReadMode
{-# INLINABLE openFilePathForReading #-}

-- | Combine values in two 'Applicative's with '<>'.
--
-- >>> combineApplicatives (Just "hello") (Just " world")
-- Just "hello world"
--
-- >>> combineApplicatives (Just "hello") Nothing
-- Nothing
combineApplicatives :: (Applicative f, Semigroup a) => f a -> f a -> f a
combineApplicatives action1 action2 =
  (<>) <$> action1 <*> action2
{-# INLINABLE combineApplicatives #-}

-- | Handle an 'IOException' that occurs when reading from a 'Handle'.  Check
-- if the 'IOException' is an EOF exception ('hIsEOF').  If so, then just close
-- the 'Handle'.  Otherwise, throw the 'IOException' that is passed in.
closeHandleIfEOFOrThrow :: MonadIO m => Handle -> IOException -> m ()
closeHandleIfEOFOrThrow handle ioerr = liftIO $ do
  isEOF <- hIsEOF handle
  if isEOF
    then hClose handle
    else ioError ioerr
{-# INLINABLE closeHandleIfEOFOrThrow #-}

-- | Call 'exitWith' with 'ExitFailure'
die
  :: Int     -- ^ exit code
  -> String  -- ^ error message to print to console
  -> IO a
die exitCode msg = do
  putStrLn $ "ERROR: " <> msg
  exitWith $ ExitFailure exitCode
