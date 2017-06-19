
module Highlight.Common.Error where

import Prelude ()
import Prelude.Compat

import Highlight.Common.Options (RawRegex)

data FileErr
  = FileAlreadyInUseErr FilePath
  | FileDoesNotExistErr FilePath
  | FilePermissionErr FilePath

data HighlightErr
  = HighlightRegexCompileErr RawRegex
