
module Highlight.Error where

import Highlight.Options (RawRegex)

data HighlightErr =
  HighlightRegexCompileErr RawRegex
