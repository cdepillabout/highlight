{-# LANGUAGE OverloadedStrings #-}

module Highlight.Run where

import qualified Data.ByteString as ByteString
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Text.RE.PCRE ((*=~))
import Text.RE.Replace (replaceAll)

import Highlight.Options (Options(..), RegEx(unRegEx))

die :: Int -> String -> IO a
die exitCode msg = undefined

handleErr :: HighlightErr -> IO a
handleErr (HighlightRegExCompileErr regex) =
  die 10 $ "Regex not well formed: " <> regex

run :: Options -> IO ()
run opts = do
  eitherRes <- runHighlightT opts prog
  either handleErr pure eitherRes

prog :: HighlightM ()
prog = undefined

-- run opts = do
--   let re = unRegEx $ optionsRegEx opts
--       lala = encodeUtf8 $ pack "this is a bytestring\nthin im 日本語 bytestring"
--       matches = lala *=~ re
--       repla = replaceAll "($0)" matches
--   ByteString.putStrLn lala
--   ByteString.putStrLn repla
