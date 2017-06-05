
module Highlight where

import Data.Monoid ((<>))
import Options.Applicative
       (InfoMod, ParserInfo, (<**>), execParser, fullDesc, header, helper,
        info, progDesc)

import Highlight.Options (Options, optionsParser)
import Highlight.Run (run)

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
      progDesc "Print a greeting for TARGET" <>
      header "hello - a test for optparse-applicative"
