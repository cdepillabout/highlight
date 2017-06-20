{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlight.Highlight.Run where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Char8
import Data.IntMap.Strict (IntMap, (!), fromList)
import Data.Monoid ((<>))
import Pipes (Producer, (>->), await, runEffect, yield)
import Text.RE.PCRE
       (RE, SimpleREOptions(MultilineInsensitive, MultilineSensitive),
        (*=~), compileRegexWith)
import Text.RE.Replace (replaceAll)

import Highlight.Common.Color
       (colorReset, colorVividBlueBold, colorVividCyanBold,
        colorVividGreenBold, colorVividMagentaBold, colorVividRedBold,
        colorVividWhiteBold)
import Highlight.Common.Error (handleErr)
import Highlight.Common.Pipes (stderrConsumer)
import Highlight.Highlight.Monad
       (FilenameHandlingFromStdin(..), FilenameHandlingFromFiles(..),
        FromGrepFilenameState, HighlightM, InputData,
        Output(OutputStderr, OutputStdout), createInputData,
        getIgnoreCaseM, getRawRegexM, handleInputData, outputConsumer,
        runHighlightM, throwRegexCompileErr, updateFilename)
import Highlight.Highlight.Options
       (IgnoreCase(IgnoreCase, DoNotIgnoreCase), Options(..),
        RawRegex(RawRegex))

run :: Options -> IO ()
run opts = do
  eitherRes <- runHighlightM opts prog
  either handleErr return eitherRes

prog :: HighlightM ()
prog = do
  outputProducer <- progOutputProducer
  runOutputProducer outputProducer

progOutputProducer :: HighlightM (Producer Output HighlightM ())
progOutputProducer = do
  regex <- compileHighlightRegexWithErr
  inputData <- createInputData
  return $ getOutputProducer regex inputData

getOutputProducer
  :: RE
  -> InputData HighlightM ()
  -> Producer Output HighlightM ()
getOutputProducer regex inputData =
  handleInputData
    (handleStdinInput regex)
    (handleFileInput regex)
    handleError
    inputData

runOutputProducer :: Producer Output HighlightM () -> HighlightM ()
runOutputProducer producer =
  runEffect $ producer >-> outputConsumer

handleStdinInput
  :: MonadState FromGrepFilenameState m
  => RE -> FilenameHandlingFromStdin -> ByteString -> m [ByteString]
handleStdinInput regex FromStdinNoFilename input =
  return $ formatNormalLine regex input
handleStdinInput regex FromStdinParseFilenameFromGrep input = do
  let (beforeColon, colonAndAfter) =
        Data.ByteString.Char8.break (== ':') input
  if colonAndAfter == empty
    then return $ formatNormalLine regex input
    else do
      let filePath = beforeColon
          lineWithoutColon = Data.ByteString.Char8.drop 1 colonAndAfter
      fileNumber <- updateFilename filePath
      return $ formatLineWithFilename regex fileNumber filePath lineWithoutColon

formatLineWithFilename
  :: RE -> Int -> ByteString -> ByteString -> [ByteString]
formatLineWithFilename regex fileNumber filePath input =
  [colorForFileNumber fileNumber
  , filePath
  , colorVividWhiteBold
  ,  ": "
  , colorReset
  , highlightMatchInRed regex input
  ]

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

handleError
  :: ByteString
  -> IOException
  -> Maybe IOException
  -> [ByteString]
handleError filePath _ (Just _) =
  ["Error when trying to read file or directory \"", filePath , "\""]
handleError filePath _ Nothing =
  ["Error when trying to read file \"", filePath , "\""]

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
