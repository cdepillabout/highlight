{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Hrep.Run where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException)
import Control.Monad.State (MonadState)
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Char8
import Data.IntMap.Strict (IntMap, (!), fromList)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid ((<>))
import System.Exit (ExitCode(ExitFailure), exitWith)
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        (*=~), compileRegexWith)
import Text.RE.Replace (replaceAll)

import Highlight.Common.Color
       (colorReset, colorVividBlueBold, colorVividCyanBold,
        colorVividGreenBold, colorVividMagentaBold, colorVividRedBold,
        colorVividWhiteBold)
import Highlight.Common.Error (HighlightErr(..))
import Highlight.Common.Options
       (IgnoreCase(IgnoreCase, DoNotIgnoreCase), CommonOptions(..),
        RawRegex(RawRegex))
import Highlight.Hrep.Monad
       (FilenameHandlingFromFiles(..), HighlightM, createInputData,
        getIgnoreCaseM, getRawRegexM, handleInputData, runHighlightM,
        throwRegexCompileErr)

die :: Int -> String -> IO a
die exitCode msg = do
  putStrLn $ "ERROR: " <> msg
  exitWith $ ExitFailure exitCode

handleErr :: HighlightErr -> IO a
handleErr (HighlightRegexCompileErr (RawRegex regex)) =
  die 10 $ "Regex not well formed: " <> regex

run :: CommonOptions -> IO ()
run opts = do
  eitherRes <- runHighlightM opts prog
  either handleErr return eitherRes

prog :: HighlightM ()
prog = do
  regex <- compileHighlightRegexWithErr
  inputData <- createInputData
  handleInputData
    (handleStdinInput regex)
    (handleFileInput regex)
    handleError
    inputData
  return ()

handleStdinInput
  :: RE -> ByteString -> NonEmpty ByteString
handleStdinInput regex input =
  formatNormalLine regex input

formatLineWithFilename
  :: RE -> Int -> ByteString -> ByteString -> NonEmpty ByteString
formatLineWithFilename regex fileNumber filePath input =
  colorForFileNumber fileNumber :|
    [ filePath
    , colorVividWhiteBold
    ,  ": "
    , colorReset
    , highlightMatchInRed regex input
    ]

formatNormalLine :: RE -> ByteString -> NonEmpty ByteString
formatNormalLine regex input =
  highlightMatchInRed regex input :| []

handleFileInput
  :: RE
  -> FilenameHandlingFromFiles
  -> ByteString
  -> Int
  -> ByteString
  -> NonEmpty ByteString
handleFileInput regex NoFilename _ _ input =
  formatNormalLine regex input
handleFileInput regex PrintFilename filePath fileNumber input =
  formatLineWithFilename regex fileNumber filePath input

handleError
  :: ByteString
  -> IOException
  -> Maybe IOException
  -> NonEmpty ByteString
handleError filePath _ (Just _) =
  "Error when trying to read file or directory \"" :| [filePath , "\""]
handleError filePath _ Nothing =
  "Error when trying to read file \"" :| [filePath , "\""]

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
  ignoreCase <- getIgnoreCaseM
  rawRegex <- getRawRegexM
  case compileHighlightRegex ignoreCase rawRegex of
    Just re -> return re
    Nothing -> throwRegexCompileErr rawRegex

compileHighlightRegex :: IgnoreCase -> RawRegex -> Maybe RE
compileHighlightRegex ignoreCase (RawRegex rawRegex) =
  let simpleREOptions =
        case ignoreCase of
          IgnoreCase -> MultilineInsensitive
          DoNotIgnoreCase -> MultilineSensitive
  in compileRegexWith simpleREOptions rawRegex
