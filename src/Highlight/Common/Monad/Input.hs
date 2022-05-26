{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Highlight.Common.Monad.Input
  ( FileOrigin(..)
  , FileReader(..)
  , getFileOriginFromFileReader
  , getFilePathFromFileReader
  , InputData(..)
  , createInputData
  , FilenameHandlingFromFiles(..)
  ) where

import Prelude ()
import Prelude.Compat

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.List (sort)
import Pipes
       (Pipe, Producer, Proxy, (>->), await, each, for, next,
        yield)
import Pipes.Prelude (toListM)
import qualified Pipes.Prelude as Pipes
import System.IO (Handle)

import Highlight.Common.Options
       (InputFilename(unInputFilename), Recursive(Recursive))
import Highlight.Pipes (childOf, fromHandleLines)
import Highlight.Util (combineApplicatives, openFilePathForReading)

-----------
-- Pipes --
-----------

-- | Place where a file originally came from.
data FileOrigin
  = FileSpecifiedByUser FilePath
  -- ^ File was specified on the command line by the user.
  | FileFoundRecursively FilePath
  -- ^ File was found recursively (not directly specified by the user).
  | Stdin
  -- ^ Standard input.  It was either specified on the command line as @-@, or
  -- used as default because the user did not specify any files.
  deriving (Eq, Read, Show)

-- | Get a 'FilePath' from a 'FileOrigin'.
--
-- >>> getFilePathFromFileOrigin $ FileSpecifiedByUser "hello.txt"
-- Just "hello.txt"
-- >>> getFilePathFromFileOrigin $ FileFoundRecursively "bye.txt"
-- Just "bye.txt"
-- >>> getFilePathFromFileOrigin Stdin
-- Nothing
getFilePathFromFileOrigin :: FileOrigin -> Maybe FilePath
getFilePathFromFileOrigin (FileSpecifiedByUser fp) = Just fp
getFilePathFromFileOrigin (FileFoundRecursively fp) = Just fp
getFilePathFromFileOrigin Stdin = Nothing

-- | This is used in two different places.
--
-- One is in 'fileListProducer', where @a@ becomes 'Handle'.  This represents a
-- single file that has been opened.  'FileReaderSuccess' contains the
-- 'FileOrigin' and the 'Handle'.  'FileReaderErr' contains the 'FileOrigin'
-- and any errors that occurred when trying to open the 'Handle'.
--
-- The other is in 'fileReaderHandleToLine' and 'InputData', where @a@ becomes
-- 'ByteString'.  This represents a single 'ByteString' line from a file, or an
-- error that occurred when trying to read the file.
--
-- 'FileReader' is usually wrapped in a 'Producer'.  This is a stream of either
-- 'Handle's or 'ByteString' lines (with any errors that have occurred).
data FileReader a
  = FileReaderSuccess !FileOrigin !a
  | FileReaderErr !FileOrigin !IOException !(Maybe IOException)
  deriving (Eq, Show)

-- | Get a 'FileOrigin' from a 'FileReader'.
--
-- >>> let fileOrigin1 = FileSpecifiedByUser "hello.txt"
-- >>> let fileReader1 = FileReaderSuccess fileOrigin1 "some line"
-- >>> getFileOriginFromFileReader fileReader1
-- FileSpecifiedByUser "hello.txt"
--
-- >>> let fileOrigin2 = FileFoundRecursively "bye.txt"
-- >>> let fileReader2 = FileReaderErr fileOrigin2 (userError "err") Nothing
-- >>> getFileOriginFromFileReader fileReader2
-- FileFoundRecursively "bye.txt"
getFileOriginFromFileReader :: FileReader a -> FileOrigin
getFileOriginFromFileReader (FileReaderSuccess origin _) = origin
getFileOriginFromFileReader (FileReaderErr origin _ _) = origin

-- | This is just
-- @'getFilePathFromFileOrigin' '.' 'getFileOriginFromFileReader'@.
--
-- >>> let fileOrigin1 = Stdin
-- >>> let fileReader1 = FileReaderSuccess fileOrigin1 "some line"
-- >>> getFilePathFromFileReader fileReader1
-- Nothing
--
-- >>> let fileOrigin2 = FileFoundRecursively "bye.txt"
-- >>> let fileReader2 = FileReaderErr fileOrigin2 (userError "err") Nothing
-- >>> getFilePathFromFileReader fileReader2
-- Just "bye.txt"
getFilePathFromFileReader :: FileReader a -> Maybe FilePath
getFilePathFromFileReader =
  getFilePathFromFileOrigin . getFileOriginFromFileReader

-- | This wraps up two pieces of information.
--
-- One is the value of 'FilenameHandlingFromFiles'.  This signals as to whether
-- or not we need to print the filename when printing each line of output.
--
-- This other is a 'Producer' of 'FileReader' 'ByteString's.  This is a
-- 'Producer' for each line of each input file.
--
-- The main job of this module is to define 'createInputData', which produces
-- 'InputData'.  'InputData' is what is processed to figure out what to output.
data InputData m a
  = InputData
      !FilenameHandlingFromFiles
      !(Producer (FileReader ByteString) m a)

-- | Create 'InputData' based 'InputFilename' list.
--
-- Setup for examples:
--
-- >>> :set -XOverloadedStrings
-- >>> import Highlight.Common.Options (InputFilename(InputFilename))
-- >>> import Highlight.Common.Options (Recursive(NotRecursive))
--
-- If the 'InputFilename' list is empty, just create an 'InputData' with
-- 'NoFilename' and the standard input 'Producer' passed in.
--
-- >>> let stdinProd = yield ("hello" :: ByteString)
-- >>> let create = createInputData NotRecursive [] stdinProd
-- >>> InputData NoFilename prod <- create
-- >>> toListM prod
-- [FileReaderSuccess Stdin "hello"]
--
-- If the 'InputFilename' list is not empty, create an 'InputData' with lines
-- from each file found on the command line.
--
-- >>> let inFiles = [InputFilename "test/golden/test-files/file1"]
-- >>> let create = createInputData NotRecursive inFiles stdinProd
-- >>> InputData NoFilename prod <- create
-- >>> Pipes.head prod
-- Just (FileReaderSuccess (FileSpecifiedByUser "test/golden/test-files/file1") "The...")
createInputData
  :: forall m.
     MonadIO m
  => Recursive
  -- ^ Whether or not to recursively read in files.
  -> [InputFilename]
  -- ^ List of files passed in on the command line.
  -> Producer ByteString m ()
  -- ^ A producer for standard input
  -> m (InputData m ())
createInputData recursive inputFilenames stdinProducer = do
  let fileOrigins = FileSpecifiedByUser . unInputFilename <$> inputFilenames
  case fileOrigins of
    [] ->
      return $
        InputData NoFilename (stdinProducerToFileReader stdinProducer)
    _ -> do
      let fileListProducers = fmap (fileListProducer recursive) fileOrigins
          fileProducer = foldl1 combineApplicatives fileListProducers
      (filenameHandling, newFileProducer) <-
        computeFilenameHandlingFromFiles fileProducer
      let fileLineProducer = fileReaderHandleToLine newFileProducer
      return $ InputData filenameHandling fileLineProducer

-- | Change a given 'Producer' into a 'FileReader' 'Producer' with the
-- 'FileOrigin' set to 'Stdin'.
--
-- You can think of this function as having the following type:
--
-- @
--   'stdinProducerToFileReader'
--     :: 'Monad' m
--     => 'Producer' a m r
--     -> 'Producer' ('FileReader' a) m r
-- @
--
-- >>> Pipes.head . stdinProducerToFileReader $ yield "hello"
-- Just (FileReaderSuccess Stdin "hello")
stdinProducerToFileReader
  :: forall x' x a m r.
     Monad m
  => Proxy x' x () a m r
  -> Proxy x' x () (FileReader a) m r
stdinProducerToFileReader producer = producer >-> Pipes.map go
  where
    go :: a -> FileReader a
    go = FileReaderSuccess Stdin
{-# INLINABLE stdinProducerToFileReader #-}

-- | Convert a 'Producer' of 'FileReader' 'Handle' into a 'Producer' of
-- 'FileReader' 'ByteString', where each line from the 'Handle' is 'yield'ed.
--
-- You can think of this function as having the following type:
--
-- @
--   'fileReaderHandleToLine'
--     :: 'MonadIO' m
--     => 'Producer' ('FileReader' 'Handle') m r
--     -> 'Producer' ('FileReader' 'ByteString') m r
-- @
--
-- >>> let fileOrigin = FileSpecifiedByUser "test/golden/test-files/file2"
-- >>> let producer = fileListProducer Recursive fileOrigin
-- >>> Pipes.head $ fileReaderHandleToLine producer
-- Just (FileReaderSuccess (FileSpecifiedByUser "test/golden/test-files/file2") "Pr...
fileReaderHandleToLine
  :: forall m x' x r.
     MonadIO m
  => Proxy x' x () (FileReader Handle) m r
  -> Proxy x' x () (FileReader ByteString) m r
fileReaderHandleToLine producer = producer >-> pipe
  where
    pipe :: Pipe (FileReader Handle) (FileReader ByteString) m r
    pipe = do
      fileReaderHandle <- await
      case fileReaderHandle of
        FileReaderErr fileOrigin fileErr dirErr ->
          yield $ FileReaderErr fileOrigin fileErr dirErr
        FileReaderSuccess fileOrigin handle -> do
          let linesProducer = fromHandleLines handle
          linesProducer >-> Pipes.map (FileReaderSuccess fileOrigin)
      pipe

-- | Create a 'Producer' of 'FileReader' 'Handle' for a given 'FileOrigin'.
--
-- Setup for examples:
--
-- >>> import Highlight.Common.Options (Recursive(NotRecursive, Recursive))
--
-- If 'NoRecursive' is specified, just try to read 'FileOrigin' as a file.
--
-- >>> let fileOrigin1 = FileSpecifiedByUser "test/golden/test-files/file2"
-- >>> toListM $ fileListProducer NotRecursive fileOrigin1
-- [FileReaderSuccess (FileSpecifiedByUser "test/.../file2") {handle: test/.../file2}]
--
-- If the file cannot be read, return an error.
--
-- >>> let fileOrigin2 = FileSpecifiedByUser "thisfiledoesnotexist"
-- >>> toListM $ fileListProducer NotRecursive fileOrigin2
-- [FileReaderErr (FileSpecifiedByUser "thisfiledoesnotexist") thisfiledoesnotexist: openBinaryFile: does not exist (No such file or directory) Nothing]
--
-- If 'Recursive' is specified, then try to read 'FileOrigin' as a directory'.
--
-- >>> let fileOrigin3 = FileSpecifiedByUser "test/golden/test-files/dir2"
-- >>> toListM $ fileListProducer Recursive fileOrigin3
-- [FileReaderSuccess (FileFoundRecursively "test/.../dir2/file6") {handle: test/.../dir2/file6}]
--
-- If the directory cannot be read, return an error.
--
-- >>> let fileOrigin4 = FileSpecifiedByUser "thisdirdoesnotexist"
-- >>> toListM $ fileListProducer Recursive fileOrigin4
-- [FileReaderErr (FileSpecifiedByUser "thisdirdoesnotexist") thisdirdoesnotexist: openBinaryFile: does not exist (No such file or directory) (Just ...)]
fileListProducer
  :: forall x' x m.
     MonadIO m
  => Recursive
  -> FileOrigin
  -- -> Producer' (FileReader Handle) m ()
  -> Proxy x' x () (FileReader Handle) m ()
fileListProducer recursive = go
  where
    go
      :: FileOrigin
      -- -> Producer' (FileReader Handle) m ()
      -> Proxy x' x () (FileReader Handle) m ()
    go fileOrigin = do
      let maybeFilePath = getFilePathFromFileOrigin fileOrigin
      case maybeFilePath of
        -- This is standard input.  We don't currently handle this, so just
        -- return unit.
        Nothing -> return ()
        -- This is a normal file.  Not standard input.
        Just filePath -> do
          eitherHandle <- openFilePathForReading filePath
          case eitherHandle of
            Right handle -> yield $ FileReaderSuccess fileOrigin handle
            Left fileIOErr ->
              if recursive == Recursive
                then do
                  let fileListM = toListM $ childOf filePath
                  eitherFileList <- liftIO $ try fileListM
                  case eitherFileList of
                    Left dirIOErr ->
                      yield $
                        FileReaderErr fileOrigin fileIOErr (Just dirIOErr)
                    Right fileList -> do
                      let sortedFileList = sort fileList
                      let fileOrigins = fmap FileFoundRecursively sortedFileList
                      let lalas =
                            fmap
                              (fileListProducer recursive)
                              fileOrigins
                      for (each lalas) id
                else
                  yield $ FileReaderErr fileOrigin fileIOErr Nothing

-----------------------
-- Filename Handling --
-----------------------

-- | This data type specifies how printing filenames will be handled, along
-- with the 'computeFilenameHandlingFromFiles' function.
data FilenameHandlingFromFiles
  = NoFilename -- ^ Do not print the filename on stdout.
  | PrintFilename -- ^ Print the filename on stdout.
  deriving (Eq, Read, Show)

-- | Given a 'Producer' of 'FileReader's, figure out whether or not we should
-- print the filename of the file to stdout.
--
-- The following examples walk through possible command lines using @highlight@, and the corresponding return values of this function.
--
-- @
--   $ highlight expression
-- @
--
-- We want to read from stdin.  There should be no 'FileReaders' in the
-- 'Producer'. Do not print the filename.
--
-- >>> let producerStdin = each []
-- >>> (fhStdin, _) <- computeFilenameHandlingFromFiles producerStdin
-- >>> fhStdin
-- NoFilename
--
-- @
--   $ highlight expression file1
-- @
--
-- We want to highlight a single file.  There should only be a single
-- 'FileReader' in the 'Producer', and it should be 'FileSpecifiedByUser'.  Do
-- not print the filename.
--
-- >>> let fileOriginSingleFile = FileSpecifiedByUser "file1"
-- >>> let fileReaderSingleFile = FileReaderSuccess fileOriginSingleFile "hello"
-- >>> let producerSingleFile = each [fileReaderSingleFile]
-- >>> (fhSingleFile, _) <- computeFilenameHandlingFromFiles producerSingleFile
-- >>> fhSingleFile
-- NoFilename
--
-- @
--   $ highlight expression file1 file2
-- @
--
-- We want to highlight two files.  Print the filename.
--
-- >>> let fileOriginMulti1 = FileSpecifiedByUser "file1"
-- >>> let fileReaderMulti1 = FileReaderSuccess fileOriginMulti1 "hello"
-- >>> let fileOriginMulti2 = FileSpecifiedByUser "file2"
-- >>> let fileReaderMulti2 = FileReaderSuccess fileOriginMulti2 "bye"
-- >>> let producerMultiFile = each [fileReaderMulti1, fileReaderMulti2]
-- >>> (fhMultiFile, _) <- computeFilenameHandlingFromFiles producerMultiFile
-- >>> fhMultiFile
-- PrintFilename
--
-- @
--   $ highlight -r expression dir1
-- @
--
-- We want to highlight all files found in @dir1\/@.  Print filenames.
--
-- >>> let fileOriginRec = FileFoundRecursively "dir1/file1"
-- >>> let fileReaderRec = FileReaderSuccess fileOriginRec "cat"
-- >>> let producerRec = each [fileReaderRec]
-- >>> (fhRec, _) <- computeFilenameHandlingFromFiles producerRec
-- >>> fhRec
-- PrintFilename
computeFilenameHandlingFromFiles
  :: forall a m r.
     Monad m
  => Producer (FileReader a) m r
  -> m (FilenameHandlingFromFiles, Producer (FileReader a) m r)
computeFilenameHandlingFromFiles producer1 = do
  eitherFileReader1 <- next producer1
  case eitherFileReader1 of
    Left ret ->
      return (NoFilename, return ret)
    Right (fileReader1, producer2) -> do
      let fileOrigin1 = getFileOriginFromFileReader fileReader1
      case fileOrigin1 of
        Stdin -> error "Not currenty handling stdin..."
        FileSpecifiedByUser _ -> do
          eitherSecondFile <- next producer2
          case eitherSecondFile of
            Left ret2 ->
              return (NoFilename, yield fileReader1 *> return ret2)
            Right (fileReader2, producer3) ->
              return
                ( PrintFilename
                , yield fileReader1 *> yield fileReader2 *> producer3
                )
        FileFoundRecursively _ ->
          return (PrintFilename, yield fileReader1 *> producer2)
