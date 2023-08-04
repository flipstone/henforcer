module Main
  ( main
  ) where

import qualified Config.Initialize as Initialize
import qualified Henforcer.Options as Options

main :: IO ()
main = do
  opts <- Options.parseOptions
  if Options.initialize opts
    then Initialize.initializeConfigPath (Options.configPath opts)
    else putStrLn "The only option currently supported for the executable usage is init."
