module Main
  ( main
  ) where

import qualified Test.Tasty as Tasty

import qualified Test.Config as TestConfig

import qualified Henforcer.Config as Config

main :: IO ()
main = do
  conf <- Config.loadConfig "test/test-config.toml"

  Tasty.defaultMain $ TestConfig.configTests conf
