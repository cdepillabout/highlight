{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Golden where

import Prelude ()
import Prelude.Compat

import Control.Exception (bracket_, try)
import Control.Lens ((&), (.~))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Pipes (Pipe, Producer, (>->), each)
import Pipes.Prelude (mapFoldable, toListM)
import System.Directory (removePathForcibly)
import System.Exit (ExitCode(ExitSuccess))
import System.IO (IOMode(WriteMode), hClose, openBinaryFile)
import System.IO.Error (isPermissionError)
#ifdef mingw32_HOST_OS
#else
import System.Posix (nullFileMode, setFileMode)
#endif
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Golden (goldenVsString)

import Highlight.Common.Pipes (fromFileLines, stdinLines)
import Highlight.Common.Util (convertStringToRawByteString)
import Highlight.Highlight.Monad
       (HighlightM, Output(OutputStderr, OutputStdout), runHighlightM)
import Highlight.Highlight.Options
       (ColorGrepFilenames(ColorGrepFilenames), IgnoreCase(IgnoreCase),
        Options, Recursive(Recursive), colorGrepFilenamesLens,
        defaultOptions, ignoreCaseLens, inputFilenamesLens, rawRegexLens,
        recursiveLens)
import Highlight.Highlight.Run (progOutputProducer)

runHighlightTestWithStdin
  :: Options
  -> (Producer ByteString HighlightM ())
  -> (forall m. Monad m => Pipe Output ByteString m ())
  -> IO LByteString.ByteString
runHighlightTestWithStdin opts stdinPipe filterPipe = do
  eitherByteStrings <- runHighlightM opts $ do
    outputProducer <- progOutputProducer stdinPipe
    toListM $ outputProducer >-> filterPipe
  case eitherByteStrings of
    Left err -> error $ "unexpected error: " <> show err
    Right byteStrings -> return . fromStrict $ fold byteStrings

runHighlightTest
  :: Options
  -> (forall m. Monad m => Pipe Output ByteString m ())
  -> IO LByteString.ByteString
runHighlightTest opts =
  runHighlightTestWithStdin opts stdinLines

filterStdout :: Monad m => Pipe Output ByteString m ()
filterStdout = mapFoldable f
  where
    f :: Output -> Maybe ByteString
    f (OutputStderr _) = Nothing
    f (OutputStdout byteString) = Just byteString

filterStderr :: Monad m => Pipe Output ByteString m ()
filterStderr = mapFoldable f
  where
    f :: Output -> Maybe ByteString
    f (OutputStderr byteString) = Just byteString
    f (OutputStdout _) = Nothing

testHighlightStderrAndStdout
  :: String
  -> FilePath
  -> ( ( forall m. Monad m => Pipe Output ByteString m ())
      -> IO LByteString.ByteString
     )
  -> TestTree
testHighlightStderrAndStdout msg path runner =
  testGroup
    msg
    [ goldenVsString "stderr" (path <> ".stderr") (runner filterStderr)
    , goldenVsString "stdout" (path <> ".stdout") (runner filterStdout)
    ]

goldenTests :: TestTree
goldenTests =
  withResource createUnreadableFile (const deleteUnreadableFile) $
    const (testGroup "golden tests" [highlightGoldenTests])

createUnreadableFile :: IO ()
createUnreadableFile = do
#ifdef mingw32_HOST_OS
  error "not yet implemented on Windows.  Please send a PR."
#else
  eitherHandle <- try $ openBinaryFile unreadableFilePath WriteMode
  case eitherHandle of
    Right handle -> do
      hClose handle
      setFileMode unreadableFilePath nullFileMode
    Left ioerr
      | isPermissionError ioerr ->
        -- assume that the file already exists, and just try to make sure that
        -- the permissions are null
        setFileMode unreadableFilePath nullFileMode
      | otherwise ->
        -- we shouldn't have gotten an error here, so just rethrow it
        ioError ioerr
#endif

deleteUnreadableFile :: IO ()
deleteUnreadableFile = removePathForcibly unreadableFilePath

unreadableFilePath :: FilePath
unreadableFilePath = "test/golden/test-files/dir2/unreadable-file"

highlightGoldenTests :: TestTree
highlightGoldenTests =
  testGroup
    "highlight"
    [ testHighlightSingleFile
    , testHighlightMultiFile
    , testHighlightFromGrep
    ]

testHighlightSingleFile :: TestTree
testHighlightSingleFile =
  let opts =
        defaultOptions
          & rawRegexLens .~ "or"
          & inputFilenamesLens .~ ["test/golden/test-files/file1"]
  in testHighlightStderrAndStdout
      "`highlight or 'test/golden/test-files/file1'`"
      "test/golden/golden-files/highlight/single-file"
      (runHighlightTest opts)

testHighlightMultiFile :: TestTree
testHighlightMultiFile =
  let opts =
        defaultOptions
          & rawRegexLens .~ "and"
          & ignoreCaseLens .~ IgnoreCase
          & recursiveLens .~ Recursive
          & inputFilenamesLens .~
              [ "test/golden/test-files/dir1"
              , "test/golden/test-files/dir2"
              ]
      testName =
        "`touch 'test/golden/test-files/dir2/unreadable-file' ; " <>
        "chmod 0 'test/golden/test-files/dir2/unreadable-file' ; " <>
        "highlight --ignore-case --recursive and " <>
          "'test/golden/test-files/dir1' 'test/golden/test-files/dir2' ; " <>
        "rm -rf 'test/golden/test-files/dir2/unreadable-file'"
  in testHighlightStderrAndStdout
      testName
      "test/golden/golden-files/highlight/multi-file"
      (runHighlightTest opts)

testHighlightFromGrep :: TestTree
testHighlightFromGrep =
  let opts =
        defaultOptions
          & rawRegexLens .~ "and"
          & colorGrepFilenamesLens .~ ColorGrepFilenames
      testName =
        "cat test/golden/test-files/grep-output | " <>
        "highlight --from-grep and`"
  in testHighlightStderrAndStdout
      testName
      "test/golden/golden-files/highlight/from-grep"
      (go opts)
  where
    go
      :: Options
      -> (forall m. Monad m => Pipe Output ByteString m ())
      -> IO LByteString.ByteString
    go opts outputPipe = do
      grepOutputProducer <- getGrepOutputProducer
      runHighlightTestWithStdin opts grepOutputProducer outputPipe

getGrepOutputProducer :: IO (Producer ByteString HighlightM ())
getGrepOutputProducer = do
  eitherProducer <- fromFileLines grepOutputTestFile
  case eitherProducer of
    Left ioerr ->
      error $
        "ERROR: following error occured when trying to read \"" <>
        grepOutputTestFile <>
        "\": " <>
        show ioerr
    Right producer -> return producer

-- | This is the output file from @grep@ to use for the test
-- 'testHighlightFromGrep'.
--
-- This file was created with the following command:
--
-- > $ grep --recursive and 'test/golden/test-files/dir1'
grepOutputTestFile :: FilePath
grepOutputTestFile = "test/golden/test-files/from-grep"
