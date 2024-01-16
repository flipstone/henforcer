module Config.Initialize
  ( initializeConfigPath
  ) where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir

import qualified Config.Defaults as Defaults

initializeConfigPath :: FilePath -> IO ()
initializeConfigPath configPath = do
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
