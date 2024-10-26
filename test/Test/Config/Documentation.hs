{-# LANGUAGE OverloadedStrings #-}

module Test.Config.Documentation
  ( checkDocumentation
  ) where

import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified CompatGHC
import qualified Henforcer.Checks as Checks
import qualified Henforcer.Config as Config
import qualified Henforcer.Rules as Rules

singleProperty :: Tasty.TestName -> HH.PropertyT IO () -> Tasty.TestTree
singleProperty name =
  TastyHH.testProperty name . HH.withTests 1 . HH.property

checkDocumentation :: Config.Config -> Tasty.TestTree
checkDocumentation conf =
  Tasty.testGroup
    "Checking configuration parsing of documentation rules"
    [ checkAnyModule conf
    , checkPatternModule conf
    , checkPatternModulePicksFirst conf
    , checkSpecifiedModule conf
    , checkPatternModuleIgnores conf
    ]

checkAnyModule :: Config.Config -> Tasty.TestTree
checkAnyModule conf =
  singleProperty "Does folding correctly for any module section" $
    documentationAnyModuleRules
      HH.=== Checks.determineDocumentationChecks conf (CompatGHC.mkModuleName "AnyModule")

checkPatternModule :: Config.Config -> Tasty.TestTree
checkPatternModule conf =
  singleProperty "Picks pattern module section when module matches" $
    documentationPatternModuleRules
      HH.=== Checks.determineDocumentationChecks conf (CompatGHC.mkModuleName "Pattern.Module")

checkPatternModulePicksFirst :: Config.Config -> Tasty.TestTree
checkPatternModulePicksFirst conf =
  singleProperty "Overlapping patterns pick the first match" $
    documentationPatternModuleRules
      HH.=== Checks.determineDocumentationChecks conf (CompatGHC.mkModuleName "Pattern2.Module")

checkSpecifiedModule :: Config.Config -> Tasty.TestTree
checkSpecifiedModule conf =
  singleProperty "Specified module is picked even if the pattern is matched" $
    documentationSpecifiedModuleRules
      HH.=== Checks.determineDocumentationChecks
        conf
        (CompatGHC.mkModuleName "PatternMatchedButSpecifiedWins.Module")

checkPatternModuleIgnores :: Config.Config -> Tasty.TestTree
checkPatternModuleIgnores conf =
  singleProperty "Pattern module can ignore rules " $
    documentationPatternWithIgnoresModuleRules
      HH.=== Checks.determineDocumentationChecks conf (CompatGHC.mkModuleName "WithIgnoresPattern.Module")

documentationAnyModuleRules :: Checks.DocumentationChecks
documentationAnyModuleRules =
  Checks.DocumentationChecks
    { Checks.maximumUndocumentedExports = Rules.Enforced 3
    , Checks.minimumDocumentedExports = Rules.Enforced 3
    , Checks.maximumExportsWithoutSince = Rules.Enforced 3
    , Checks.minimumExportsWithSince = Rules.Enforced 3
    , Checks.moduleHeaderCopyrightMustExistNonEmpty = Rules.Enforced True
    , Checks.moduleHeaderDescriptionMustExistNonEmpty = Rules.Enforced True
    , Checks.moduleHeaderLicenseMustExistNonEmpty = Rules.Enforced True
    , Checks.moduleHeaderMaintainerMustExistNonEmpty = Rules.Enforced True
    }

documentationPatternModuleRules :: Checks.DocumentationChecks
documentationPatternModuleRules =
  Checks.DocumentationChecks
    { Checks.maximumUndocumentedExports = Rules.Enforced 2
    , Checks.minimumDocumentedExports = Rules.Enforced 2
    , Checks.maximumExportsWithoutSince = Rules.Enforced 2
    , Checks.minimumExportsWithSince = Rules.Enforced 2
    , Checks.moduleHeaderCopyrightMustExistNonEmpty = Rules.Enforced False
    , Checks.moduleHeaderDescriptionMustExistNonEmpty = Rules.Enforced False
    , Checks.moduleHeaderLicenseMustExistNonEmpty = Rules.Enforced False
    , Checks.moduleHeaderMaintainerMustExistNonEmpty = Rules.Enforced False
    }

documentationPatternWithIgnoresModuleRules :: Checks.DocumentationChecks
documentationPatternWithIgnoresModuleRules =
  Checks.DocumentationChecks
    { Checks.maximumUndocumentedExports = Rules.NotEnforced
    , Checks.minimumDocumentedExports = Rules.NotEnforced
    , Checks.maximumExportsWithoutSince = Rules.NotEnforced
    , Checks.minimumExportsWithSince = Rules.NotEnforced
    , Checks.moduleHeaderCopyrightMustExistNonEmpty = Rules.NotEnforced
    , Checks.moduleHeaderDescriptionMustExistNonEmpty = Rules.NotEnforced
    , Checks.moduleHeaderLicenseMustExistNonEmpty = Rules.NotEnforced
    , Checks.moduleHeaderMaintainerMustExistNonEmpty = Rules.NotEnforced
    }

documentationSpecifiedModuleRules :: Checks.DocumentationChecks
documentationSpecifiedModuleRules =
  Checks.DocumentationChecks
    { Checks.maximumUndocumentedExports = Rules.Enforced 20
    , Checks.minimumDocumentedExports = Rules.Enforced 20
    , Checks.maximumExportsWithoutSince = Rules.Enforced 20
    , Checks.minimumExportsWithSince = Rules.Enforced 20
    , Checks.moduleHeaderCopyrightMustExistNonEmpty = Rules.Enforced True
    , Checks.moduleHeaderDescriptionMustExistNonEmpty = Rules.Enforced True
    , Checks.moduleHeaderLicenseMustExistNonEmpty = Rules.Enforced True
    , Checks.moduleHeaderMaintainerMustExistNonEmpty = Rules.Enforced True
    }
