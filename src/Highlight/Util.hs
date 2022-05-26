module Highlight.Util where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState, get, put)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import Foreign.C (newCStringLen)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (Handle, IOMode(ReadMode), hClose, hIsEOF, openBinaryFile)

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
--
-- >>> die 10 "message"
-- ERROR: message
-- *** Exception: ExitFailure 10
die
  :: Int     -- ^ exit code
  -> String  -- ^ error message to print to console
  -> IO a
die exitCode msg = do
  putStrLn $ "ERROR: " <> msg
  exitWith $ ExitFailure exitCode
{-# INLINABLE die #-}

-- | Perform an action when a list is non-null.
--
-- >>> whenNonNull [1,2,3] $ putStrLn "hello"
-- hello
-- >>> whenNonNull [] $ putStrLn "bye"
--
whenNonNull :: Monad m => [a] -> m () -> m ()
whenNonNull [] _ = return ()
whenNonNull _ action = action
{-# INLINABLE whenNonNull #-}


-- | A variant of 'modify' in which the computation is strict in the
-- new state.
--
-- * @'modify'' f = 'get' >>= (('$!') 'put' . f)@
--
-- This is used because 'modify'' is not available in the @tranformers-0.3.0.0@
-- package.
modify' :: MonadState s m => (s -> s) -> m ()
modify' f = do
  s <- get
  put $! f s
{-# INLINE modify' #-}
