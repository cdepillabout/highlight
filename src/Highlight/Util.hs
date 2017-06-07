module Highlight.Util where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Foreign.C (newCStringLen)

convertStringToRawByteString :: String -> IO ByteString
convertStringToRawByteString str = do
  -- TODO: cStringLen should really be freed after we are finished using the
  -- resulting bytestring.
  cStringLen <- newCStringLen str
  unsafePackCStringLen cStringLen
