
module Highlight.Common.Error where

import Prelude ()
import Prelude.Compat

import Data.Monoid ((<>))

import Highlight.Common.Options (RawRegex(RawRegex))
import Highlight.Common.Util (die)

data FileErr
  = FileAlreadyInUseErr FilePath
  | FileDoesNotExistErr FilePath
  | FilePermissionErr FilePath

data HighlightErr
  = HighlightRegexCompileErr RawRegex

handleErr :: HighlightErr -> IO a
handleErr (HighlightRegexCompileErr (RawRegex regex)) =
  die 10 $ "Regex not well formed: " <> regex

