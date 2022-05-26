{- |
Module      :  Highlight.Highlight

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module contains the 'defaultMain' function for the @highlight@ program.
-}

module Highlight.Highlight where

import Options.Applicative
       (InfoMod, ParserInfo, (<**>), execParser, fullDesc, helper,
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
      progDesc "Highlight PATTERN in a each FILE or standard input"
