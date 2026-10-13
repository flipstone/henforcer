module Main
  ( main
  ) where

import qualified Test.Tasty as Tasty

import qualified Henforcer.Config as Config
import qualified Test.Checks as TestChecks
import qualified Test.CodeStructure as TestCodeStructure
import qualified Test.Config as TestConfig

main :: IO ()
main = do
  conf <- Config.loadConfig "test/test-config.toml"

  Tasty.defaultMain $
    Tasty.testGroup
      "henforcer"
      [ TestChecks.checksTests
      , TestCodeStructure.codeStructureTests
      , TestConfig.configTests conf
      ]
