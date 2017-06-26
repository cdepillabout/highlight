{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Highlight.Hrep.Run where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException)
import Data.ByteString (ByteString)
import Data.Maybe (maybeToList)
import Pipes (Producer, (>->), runEffect)
import Text.RE.PCRE (RE, (*=~), anyMatches)
import Text.RE.Replace (replaceAll)

import Highlight.Common.Color
       (colorForFileNumber, colorReset, colorVividWhiteBold,
        replaceInRedByteString)
import Highlight.Common.Error (handleErr)
import Highlight.Common.Options (CommonOptions(..))
import Highlight.Common.Pipes (stdinLines)
import Highlight.Hrep.Monad
       (FilenameHandlingFromFiles(..), HrepM, InputData', Output,
        compileHighlightRegexWithErr, createInputData', getInputFilenamesM,
        getRecursiveM, handleInputData, outputConsumer, runHrepM)

run :: CommonOptions -> IO ()
run opts = do
  eitherRes <- runHrepM opts prog
  either handleErr return eitherRes

prog :: HrepM ()
prog = do
  outputProducer <- hrepOutputProducer stdinLines
  runOutputProducer outputProducer

hrepOutputProducer
  :: Producer ByteString HrepM ()
  -> HrepM (Producer Output HrepM ())
hrepOutputProducer stdinProducer = do
  regex <- compileHighlightRegexWithErr
  inputFilenames <- getInputFilenamesM
  recursive <- getRecursiveM
  inputData <- createInputData' recursive inputFilenames stdinProducer
  let outputProducer = getOutputProducer regex inputData
  return outputProducer

getOutputProducer
  :: RE
  -> InputData' HrepM ()
  -> Producer Output HrepM ()
getOutputProducer regex =
  handleInputData
    (handleStdinInput regex)
    (handleFileInput regex)
    handleError

runOutputProducer :: Producer Output HrepM () -> HrepM ()
runOutputProducer producer =
  runEffect $ producer >-> outputConsumer

handleStdinInput
  :: Monad m
  => RE -> ByteString -> m [ByteString]
handleStdinInput regex input =
  return $ formatNormalLine regex input

formatNormalLine :: RE -> ByteString -> [ByteString]
formatNormalLine regex =
  maybeToList . highlightMatchInRed regex

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

formatLineWithFilename
  :: RE -> Int -> ByteString -> ByteString -> [ByteString]
formatLineWithFilename regex fileNumber filePath input =
  case highlightMatchInRed regex input of
    Nothing -> []
    Just line ->
      [ colorForFileNumber fileNumber
      , filePath
      , colorVividWhiteBold
      ,  ": "
      , colorReset
      , line
      ]

handleError
  :: Monad m
  => ByteString
  -> IOException
  -> Maybe IOException
  -> m [ByteString]
handleError filePath _ (Just _) =
  return ["Error when trying to read file or directory \"", filePath, "\""]
handleError filePath _ Nothing =
  return ["Error when trying to read file \"", filePath, "\""]

highlightMatchInRed :: RE -> ByteString -> Maybe ByteString
highlightMatchInRed regex input =
  let matches = input *=~ regex
      didMatch = anyMatches matches
  in if didMatch
       then Just $ replaceAll replaceInRedByteString matches
       else Nothing
