module Main
  ( main
  ) where

import qualified Modulint.Main as Modulint
import qualified Modulint.Options as Options

main :: IO ()
main = do
  options <- Options.parseOptions
  Modulint.main options

