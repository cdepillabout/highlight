
module Highlight.Hrep where

import Data.Monoid ((<>))
import Options.Applicative
       (InfoMod, ParserInfo, (<**>), execParser, fullDesc, header, helper,
        info, progDesc)

import Highlight.Hrep.Options (Options, optionsParser)
import Highlight.Hrep.Run (run)

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
      progDesc "Search for PATTERN in each FILE or standard input." {- <>
      header "hello - a test for optparse-applicative" -}
