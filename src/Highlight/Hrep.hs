
module Highlight.Hrep where

import Options.Applicative
       (InfoMod, ParserInfo, (<**>), execParser, fullDesc, helper, info,
        progDesc)

import Highlight.Common.Options (CommonOptions, commonOptionsParser)
import Highlight.Hrep.Run (run)

defaultMain :: IO ()
defaultMain = do
  options <- execParser parserInfo
  run options
  where
    parserInfo :: ParserInfo CommonOptions
    parserInfo = info (commonOptionsParser <**> helper) infoMod

    infoMod :: InfoMod a
    infoMod =
      fullDesc <>
      progDesc "Search for PATTERN in each FILE or standard input."
