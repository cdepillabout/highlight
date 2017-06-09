module Highlight.Util where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Foreign.C (newCStringLen)
import System.IO.Unsafe (unsafePerformIO)

-- | Convert a String to a ByteString with the encoding for the current locale.
convertStringToRawByteString :: String -> IO ByteString
convertStringToRawByteString str = do
  -- TODO: cStringLen should really be freed after we are finished using the
  -- resulting bytestring.
  cStringLen <- newCStringLen str
  unsafePackCStringLen cStringLen

unsafeConvertStringToRawByteString :: String -> ByteString
unsafeConvertStringToRawByteString =
  unsafePerformIO . convertStringToRawByteString
