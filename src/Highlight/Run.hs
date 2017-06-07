{-# LANGUAGE OverloadedStrings #-}

module Highlight.Run where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Monoid ((<>))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Foreign.C (newCStringLen)
import Pipes (Producer)
import Pipes.ByteString (lines)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        (*=~), compileRegexWith)
import Text.RE.Replace (replaceAll)

import Highlight.Error (HighlightErr(..))
import Highlight.Monad
       (HighlightM, createInputData, getIgnoreCase, getRawRegex,
        handleInputData, runHighlightM, throwRegexCompileErr)
import Highlight.Options
       (IgnoreCase(IgnoreCase, DoNotIgnoreCase), Options(..),
        RawRegex(RawRegex))

die :: Int -> String -> IO a
die exitCode msg = do
  putStrLn $ "ERROR: " <> msg
  exitWith $ ExitFailure exitCode

handleErr :: HighlightErr -> IO a
handleErr (HighlightRegexCompileErr (RawRegex regex)) =
  die 10 $ "Regex not well formed: " <> regex

run :: Options -> IO ()
run opts = do
  eitherRes <- runHighlightM opts prog
  either handleErr pure eitherRes

prog :: HighlightM ()
prog = do
  regex <- compileHighlightRegexWithErr
  inputData <- createInputData
  handleInputData inputData
  pure ()

-- run opts = do
--   let re = unRegEx $ optionsRegEx opts
--       lala = encodeUtf8 $ pack "this is a bytestring\nthin im 日本語 bytestring"
--       matches = lala *=~ re
--       repla = replaceAll "($0)" matches
--   ByteString.putStrLn lala
--   ByteString.putStrLn repla

compileHighlightRegexWithErr :: HighlightM RE
compileHighlightRegexWithErr = do
  ignoreCase <- getIgnoreCase
  rawRegex <- getRawRegex
  case compileHighlightRegex ignoreCase rawRegex of
    Just re -> pure re
    Nothing -> throwRegexCompileErr rawRegex

compileHighlightRegex :: IgnoreCase -> RawRegex -> Maybe RE
compileHighlightRegex ignoreCase (RawRegex rawRegex) =
  let simpleREOptions =
        case ignoreCase of
          IgnoreCase -> MultilineInsensitive
          DoNotIgnoreCase -> MultilineSensitive
  in compileRegexWith simpleREOptions rawRegex
