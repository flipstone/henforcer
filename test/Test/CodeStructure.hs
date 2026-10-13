module Test.CodeStructure
  ( codeStructureTests
  ) where

import qualified Test.Tasty as Tasty

import Test.CodeStructure.PackageQualifier (packageQualifierTests)

codeStructureTests :: Tasty.TestTree
codeStructureTests =
  Tasty.testGroup
    "Code structure tests"
    [ packageQualifierTests
    ]
