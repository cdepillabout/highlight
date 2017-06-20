{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Hrep.Run where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException)
import Data.ByteString (ByteString)
import Data.IntMap.Strict (IntMap, (!), fromList)
import Data.Monoid ((<>))
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        (*=~), compileRegexWith)
import Text.RE.Replace (replaceAll)

import Highlight.Common.Color
       (colorReset, colorVividBlueBold, colorVividCyanBold,
        colorVividGreenBold, colorVividMagentaBold, colorVividRedBold,
        colorVividWhiteBold)
import Highlight.Common.Error (handleErr)
import Highlight.Common.Options
       (IgnoreCase(IgnoreCase, DoNotIgnoreCase), CommonOptions(..),
        RawRegex(RawRegex))
import Highlight.Hrep.Monad
       (FilenameHandlingFromFiles(..), HighlightM, createInputData,
        getIgnoreCaseM, getRawRegexM, handleInputData, runHighlightM,
        throwRegexCompileErr)

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
  :: RE -> ByteString -> [ByteString]
handleStdinInput regex input =
  formatNormalLine regex input

formatNormalLine :: RE -> ByteString -> [ByteString]
formatNormalLine regex input =
  [highlightMatchInRed regex input]

handleFileInput
  :: RE
  -> FilenameHandlingFromFiles
  -> ByteString
  -> Int
  -> ByteString
  -> [ByteString]
handleFileInput regex NoFilename _ _ input =
  formatNormalLine regex input
handleFileInput regex PrintFilename filePath fileNumber input =
  formatLineWithFilename regex fileNumber filePath input

formatLineWithFilename
  :: RE -> Int -> ByteString -> ByteString -> [ByteString]
formatLineWithFilename regex fileNumber filePath input =
  [ colorForFileNumber fileNumber
  , filePath
  , colorVividWhiteBold
  ,  ": "
  , colorReset
  , highlightMatchInRed regex input
  ]

handleError
  :: ByteString
  -> IOException
  -> Maybe IOException
  -> [ByteString]
handleError filePath _ (Just _) =
  [ "Error when trying to read file or directory \""
  , filePath
  , "\""
  ]
handleError filePath _ Nothing =
  [ "Error when trying to read file \""
  , filePath
  , "\""
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
