module Modulint.Initialize
  ( initializeConfigPath
  ) where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import qualified Modulint.Config.Defaults as Defaults

initializeConfigPath :: FilePath -> IO ()
initializeConfigPath configPath = do
  let
    modulintDirPath = FilePath.takeDirectory configPath </> ".modulint"
    preludePath = modulintDirPath </> "prelude.dhall"

  Dir.createDirectoryIfMissing False modulintDirPath

  preludeExists <- Dir.doesFileExist preludePath

  if
    preludeExists
  then
    putStrLn (preludePath <> " already exists, skipping. Remove your file and run init again to get the latest prelude.")
  else do
    BS.writeFile preludePath Defaults.preludeFile
    putStrLn ("Installed modulint prelude file at " <> preludePath)

  modulintExists <- Dir.doesFileExist configPath

  if
    modulintExists
  then
    putStrLn (configPath <> " already exists, skipping creation to avoid overwriting your settings. Remove your file and run init again to start over from scratch.")
  else do
    BS.writeFile configPath Defaults.initialModulintFile
    putStrLn ("Installed default modulint configuration file at " <> configPath)

