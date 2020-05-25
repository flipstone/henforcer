module Modulint.Directory
  ( foldDirectory
  ) where

import qualified Control.Monad as Monad
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import qualified System.Posix.Files as Posix

foldDirectory :: (FilePath -> Bool)
              -> (a -> FilePath -> IO a)
              -> a
              -> FilePath
              -> IO a
foldDirectory isFileOfInterest handleFile initial =
  go initial
    where
      foldEntry accum path = do
        status <- Posix.getFileStatus path

        if Posix.isDirectory status
        then go accum path
        else if Posix.isRegularFile status && isFileOfInterest path
        then handleFile accum path
        else pure accum

      go accum baseDir = do
        contents <- Dir.listDirectory baseDir

        let
          fullPaths = map (FilePath.combine baseDir) contents

        Monad.foldM foldEntry accum fullPaths
