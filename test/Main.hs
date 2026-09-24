module Main
  ( main
  ) where

import qualified Test.Tasty as Tasty

import qualified Henforcer.Config as Config
import qualified Test.Config as TestConfig

main :: IO ()
main = do
  conf <- Config.loadConfig "test/test-config.toml"

  Tasty.defaultMain $ TestConfig.configTests conf
