
module Highlight.Highlight where

import Data.Monoid ((<>))
import Options.Applicative
       (InfoMod, ParserInfo, (<**>), execParser, fullDesc, header, helper,
        info, progDesc)

import Highlight.Highlight.Options (Options, optionsParser)
import Highlight.Highlight.Run (run)

defaultMain :: IO ()
defaultMain = do
  options <- execParser parserInfo
  run options
  where
    parserInfo :: ParserInfo Options
    parserInfo = info (optionsParser <**> helper) infoMod

    infoMod :: InfoMod a
    infoMod =
      fullDesc <>
      progDesc "Highlight PATTERN in a each FILE or standard input" {- <>
      header "highlight - a test for optparse-applicative" -}
