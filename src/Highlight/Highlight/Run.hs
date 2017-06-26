{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Highlight.Highlight.Run where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString.Char8
import Pipes (Producer, (>->), runEffect)
import Text.RE.PCRE (RE, (*=~))
import Text.RE.Replace (replaceAll)

import Highlight.Common.Color
       (colorForFileNumber, colorReset, colorVividWhiteBold,
        replaceInRedByteString)
import Highlight.Common.Error (handleErr)
import Highlight.Common.Pipes (stdinLines)
import Highlight.Highlight.Monad
       (FilenameHandlingFromStdin(..), FilenameHandlingFromFiles(..),
        FromGrepFilenameState, HighlightM, InputData, Output,
        compileHighlightRegexWithErr, createInputData,
        filenameHandlingFromStdinM, getInputFilenamesM, getRecursiveM,
        handleInputData, outputConsumer, runHighlightM,
        updateFilename)
import Highlight.Highlight.Options (HasColorGrepFilenames, Options(..))

run :: Options -> IO ()
run opts = do
  eitherRes <- runHighlightM opts prog
  either handleErr return eitherRes

prog :: HighlightM ()
prog = do
  outputProducer <- highlightOutputProducer' stdinLines
  runOutputProducer outputProducer

highlightOutputProducer'
  :: Producer ByteString HighlightM ()
  -> HighlightM (Producer Output HighlightM ())
highlightOutputProducer' stdinProducer = do
  regex <- compileHighlightRegexWithErr
  inputFilenames <- getInputFilenamesM
  recursive <- getRecursiveM
  inputData <- createInputData recursive inputFilenames stdinProducer
  let outputProducer = getOutputProducer regex inputData
  return outputProducer

getOutputProducer
  :: RE
  -> InputData HighlightM ()
  -> Producer Output HighlightM ()
getOutputProducer regex =
  handleInputData
    (handleStdinInput' regex)
    (handleFileInput regex)
    handleError

runOutputProducer :: Producer Output HighlightM () -> HighlightM ()
runOutputProducer producer =
  runEffect $ producer >-> outputConsumer

handleStdinInput'
  :: ( HasColorGrepFilenames r
     , MonadState FromGrepFilenameState m
     , MonadReader r m
     )
  => RE -> ByteString -> m [ByteString]
handleStdinInput' regex input = do
  stdinHandling <- filenameHandlingFromStdinM
  case stdinHandling of
    FromStdinNoFilename -> return $ formatNormalLine regex input
    FromStdinParseFilenameFromGrep -> do
      let (beforeColon, colonAndAfter) =
            Data.ByteString.Char8.break (== ':') input
      if colonAndAfter == empty
        then return $ formatNormalLine regex input
        else do
          let filePath = beforeColon
              lineWithoutColon = Data.ByteString.Char8.drop 1 colonAndAfter
          fileNumber <- updateFilename filePath
          return $
            formatLineWithFilename regex fileNumber filePath lineWithoutColon

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

formatNormalLine :: RE -> ByteString -> [ByteString]
formatNormalLine regex input =
  [highlightMatchInRed regex input]

handleFileInput
  :: Monad m
  => RE
  -> FilenameHandlingFromFiles
  -> ByteString
  -> Int
  -> ByteString
  -> m [ByteString]
handleFileInput regex NoFilename _ _ input =
  return $ formatNormalLine regex input
handleFileInput regex PrintFilename filePath fileNumber input =
  return $ formatLineWithFilename regex fileNumber filePath input

handleError
  :: Monad m
  => ByteString -> IOException -> Maybe IOException -> m [ByteString]
handleError filePath _ (Just _) =
  return ["Error when trying to read file or directory \"", filePath , "\""]
handleError filePath _ Nothing =
  return ["Error when trying to read file \"", filePath , "\""]

highlightMatchInRed :: RE -> ByteString -> ByteString
highlightMatchInRed regex input =
  let matches = input *=~ regex
  in replaceAll replaceInRedByteString matches
