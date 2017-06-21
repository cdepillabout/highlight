module Main where

import Prelude ()
import Prelude.Compat

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Golden (goldenTests)

main :: IO ()
main = do
  -- createDirectoryIfMissing False (testDir </> "empty-dir")
  tests <- testsIO
  defaultMain tests

testsIO :: IO TestTree
testsIO = do
  return $ testGroup "tests" [goldenTests]

