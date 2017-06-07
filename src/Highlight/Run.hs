{-# LANGUAGE OverloadedStrings #-}

module Highlight.Run where

import Control.Monad.Trans.Free (FreeT)
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
       (HighlightM, getIgnoreCase, getRawRegex, runHighlightT,
        throwHighlightErr)
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
  eitherRes <- runHighlightT opts prog
  either handleErr pure eitherRes

prog :: HighlightM ()
prog = do
  regex <- compileHighlightRegexWithErr
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

convertStringToRawByteString :: String -> IO ByteString
convertStringToRawByteString str = do
  -- TODO: cStringLen should really be freed after we are finished using the
  -- resulting bytestring.
  cStringLen <- newCStringLen str
  unsafePackCStringLen cStringLen

compileHighlightRegex :: IgnoreCase -> RawRegex -> Maybe RE
compileHighlightRegex ignoreCase (RawRegex rawRegex) =
  let simpleREOptions =
        case ignoreCase of
          IgnoreCase -> MultilineInsensitive
          DoNotIgnoreCase -> MultilineSensitive
  in compileRegexWith simpleREOptions rawRegex

data InputSource
  = InputSourceStdin
  | InputSourceSingleFile FilePath
  | InputSourceMultiFile FilePath

getInput
  :: HighlightM (Producer (InputSource, FreeT (Producer ByteString m) m x) m x)
getInput = do
  inputFileNames <- getInputFileNames
  recursive <- getRecursive
  case (inputFileNames, recursive) of
    ([], _) -> undefined -- from stdin
    ([singleFile], NotRecursive) ->
        producerForFile SingleFileNotRecursive singleFile
    ([singleFile], Recursive) ->
        producerForFile SingleFileRecursive singleFile
    (multiFiles, NotRecursive) -> undefined
    (multiFiles, Recursive) -> undefined

data MultiFileType
  = MultiFileNotRecursive
  | MultiFileRecursive
  | SingleFileNotRecursive
  | SingleFileRecursive

producerForFile
  :: MultiFileType
  -> FilePath
  -> Producer (InputSource, FreeT (Producer ByteString m) m x) m x
producerForFile multiFileType filePath = do
  case (multiFileType, recursive) of
    (PossiblyMultiFiles, NotRecursive) -> do
      eitherHandle <- try $ openBinaryFile filePath ReadMode
      case eitherHandler of
        Left ioerr ->
          if | isAlreadyInUseError ioerr -> throwFileAlreadyInUseErr filePath
             | isDoesNotExistError ioerr -> throwFileDoesNotExistErr filePath
             | isPermissionError ioerr -> throwFilePermissionErr filePath
             | otherwise -> throwIOerr ioerr
        Right handle -> do
          let linesFreeTProducer = fromHandle handle ^. lines
          yield (InputSourceSingleFile filePath, linesFreeTProducer)
