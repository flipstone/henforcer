module Config.Initialize
  ( initializeConfigPath
  ) where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

import qualified Config.Defaults as Defaults

initializeConfigPath :: FilePath -> IO ()
initializeConfigPath configPath = do
  let henforcerDirPath = FilePath.takeDirectory configPath </> ".henforcer"
      packagePath = henforcerDirPath </> "package.dhall"

  Dir.createDirectoryIfMissing False henforcerDirPath

  packageExists <- Dir.doesFileExist packagePath

  if packageExists
    then
      putStrLn
        ( packagePath
            <> " already exists, skipping. Remove your file and run init again to get the latest package."
        )
    else do
      BS.writeFile packagePath Defaults.packageFile
      putStrLn ("Installed henforcer package file at " <> packagePath)

  henforcerExists <- Dir.doesFileExist configPath

  if henforcerExists
    then
      putStrLn
        ( configPath
            <> " already exists, skipping creation to avoid overwriting your settings. Remove your file and run init again to start over from scratch."
        )
    else do
      BS.writeFile configPath Defaults.initialHenforcerFile
      putStrLn ("Installed default henforcer configuration file at " <> configPath)
