module Test.Checks
  ( checksTests
  ) where

import qualified Test.Tasty as Tasty

import Test.Checks.DocumentationCheck (documentationCheckTests)

checksTests :: Tasty.TestTree
checksTests =
  Tasty.testGroup
    "Checks tests"
    [ documentationCheckTests
    ]
