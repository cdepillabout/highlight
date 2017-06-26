
module Highlight.Common.Error where

import Prelude ()
import Prelude.Compat

import Data.Monoid ((<>))

import Highlight.Common.Options (RawRegex(RawRegex))
import Highlight.Util (die)

data HighlightErr
  = HighlightRegexCompileErr RawRegex
  deriving Show

handleErr :: HighlightErr -> IO a
handleErr (HighlightRegexCompileErr (RawRegex regex)) =
  die 10 $ "Regex not well formed: " <> regex
