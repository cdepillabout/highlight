
module Highlight.Common.Error where

import Prelude ()
import Prelude.Compat

import Highlight.Common.Options (RawRegex(RawRegex))
import Highlight.Util (die)

-- | Sum-type representing all errors that can be thrown by this application.
data HighlightErr
  = HighlightRegexCompileErr RawRegex
  -- ^ Error when trying to compile the 'RawRegex' into a regular expression.
  deriving Show

-- | Call 'die' with an error message based on 'HighlightErr'.
handleErr :: HighlightErr -> IO a
handleErr (HighlightRegexCompileErr (RawRegex regex)) =
  die 10 $ "Regex not well formed: " <> regex
