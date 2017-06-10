{-# LANGUAGE OverloadedStrings #-}

module Highlight.Run where

import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap, (!))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid ((<>))
import GHC.Exts (IsList(fromList))
import System.Exit (ExitCode(ExitFailure), exitWith)
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        (*=~), compileRegexWith)
import Text.RE.Replace (replaceAll)

import Highlight.Color
       (colorReset, colorVividBlueBold, colorVividCyanBold,
        colorVividGreenBold, colorVividMagentaBold, colorVividRedBold,
        colorVividWhiteBold)
import Highlight.Error (HighlightErr(..))
import Highlight.Monad
       (FilenameHandlingFromStdin(..), FilenameHandlingFromFiles(..),
        HighlightM, createInputData, getIgnoreCase, getRawRegex,
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
  handleInputData
    (handleStdinInput regex)
    (handleFileInput regex)
    inputData
  pure ()

handleStdinInput :: RE -> FilenameHandlingFromStdin -> ByteString -> ByteString
handleStdinInput _regex FromStdinParseFilenameFromGrep _input = undefined
handleStdinInput regex FromStdinNoFilename input =
  highlightMatchInRed regex input

handleFileInput
  :: RE
  -> FilenameHandlingFromFiles
  -> ByteString
  -> Int
  -> ByteString
  -> NonEmpty ByteString
handleFileInput regex FromFilesNoFilename _ _ input =
  highlightMatchInRed regex input :| []
handleFileInput regex FromFilesPrintFilename filePath fileNumber input =
  colorForFileNumber fileNumber :|
    [ filePath
    , colorVividWhiteBold
    ,  ": "
    , colorReset
    , highlightMatchInRed regex input
    ]

highlightMatchInRed :: RE -> ByteString -> ByteString
highlightMatchInRed regex input =
  let matches = input *=~ regex
  in replaceAll replaceInRedByteString matches

colorForFileNumber :: Int -> ByteString
colorForFileNumber num = allColorsMap ! (num `mod` allColorsLength)

allColorsMap :: IntMap ByteString
allColorsMap = fromList $ zip [0..] allColorsList

allColorsLength :: Int
allColorsLength = length allColorsList

allColorsList :: [ByteString]
allColorsList =
  [ colorVividBlueBold
  , colorVividGreenBold
  , colorVividCyanBold
  , colorVividMagentaBold
  ]

replaceInRedByteString :: ByteString
replaceInRedByteString = colorVividRedBold <> "$0" <> colorReset

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
