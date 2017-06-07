
module Highlight.Error where

import Highlight.Options (RawRegex)

data FileErr
  = FileAlreadyInUseErr FilePath
  | FileDoesNotExistErr FilePath
  | FilePermissionErr FilePath

data HighlightErr
  = HighlightRegexCompileErr RawRegex
