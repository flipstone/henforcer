module Test.Config
  ( configTests
  ) where

import qualified Test.Tasty as Tasty

import qualified Henforcer.Config as Config
import Test.Config.Documentation
  ( checkDocumentation
  )
import Test.Config.Import (checkImport)

configTests :: Config.Config -> Tasty.TestTree
configTests conf =
  Tasty.testGroup
    "Configuration parsing tests"
    [ checkDocumentation conf
    , checkImport conf
    ]
