{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Golden where

import Control.Lens ((&), (.~))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as LByteString
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Pipes (Pipe, Producer, (>->))
import Pipes.Prelude (mapFoldable, toListM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Highlight.Highlight.Monad
       (HighlightM, Output(OutputStderr, OutputStdout), runHighlightM)
import Highlight.Highlight.Options
       (Options, defaultCommonOptions, defaultOptions, inputFilenamesLens,
        rawRegexLens)
import Highlight.Highlight.Run (progOutputProducer)

runHighlightTest
  :: Options
  -> (forall m. Monad m => Pipe Output ByteString m ())
  -> IO LByteString.ByteString
runHighlightTest opts filterPipe = do
  eitherByteStrings <- runHighlightM opts $ do
    outputProducer <- progOutputProducer
    toListM $ outputProducer >-> filterPipe
  case eitherByteStrings of
    Left err -> error $ "unexpected error: " <> show err
    Right byteStrings -> return . fromStrict $ fold byteStrings

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
goldenTests = testGroup "golden tests" [highlightGoldenTests]

highlightGoldenTests :: TestTree
highlightGoldenTests =
  testGroup
    "highlight"
    [ testHighlightSingleFile
    ]

testHighlightSingleFile :: TestTree
testHighlightSingleFile =
  let opts =
        defaultOptions
          & rawRegexLens .~ "or"
          & inputFilenamesLens .~ ["test/golden/test-files/file1"]
  in testHighlightStderrAndStdout
      "single file"
      "test/golden/golden-files/highlight/single-file"
      (runHighlightTest opts)
