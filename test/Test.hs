module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Golden (goldenTestsIO)

main :: IO ()
main = do
  -- createDirectoryIfMissing False (testDir </> "empty-dir")
  tests <- testsIO
  defaultMain tests

testsIO :: IO TestTree
testsIO = do
  goldenTests <- goldenTestsIO
  return $ testGroup "tests" [goldenTests]

