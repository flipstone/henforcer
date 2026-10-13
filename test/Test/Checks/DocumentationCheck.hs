module Test.Checks.DocumentationCheck
  ( documentationCheckTests
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

documentationCheckTests :: Tasty.TestTree
documentationCheckTests =
  Tasty.testGroup
    "Checking when documentation checks are possible for a configuration"
    [ emptyConfigIsNotPossible
    , importRulesOnlyIsNotPossible
    , anyModuleRuleIsPossible
    , specifiedModuleRuleIsPossible
    , patternModuleRuleIsPossible
    , ignoredPatternModuleRuleIsStillPossible
    , ruleRequiringNothingIsNotPossible
    ]

emptyConfigIsNotPossible :: Tasty.TestTree
emptyConfigIsNotPossible =
  singleProperty "An empty configuration needs no documentation information" $
    Checks.documentationChecksArePossible (configWith Config.emptyForAnyModule [] [])
      HH.=== False

importRulesOnlyIsNotPossible :: Tasty.TestTree
importRulesOnlyIsNotPossible =
  singleProperty "A configuration of only import rules needs no documentation information" $
    Checks.documentationChecksArePossible
      ( configWith
          Config.emptyForAnyModule
            { Config.anyModuleAllowedOpenUnaliasedImports = Rules.Enforced 1
            }
          []
          []
      )
      HH.=== False

anyModuleRuleIsPossible :: Tasty.TestTree
anyModuleRuleIsPossible =
  singleProperty "A rule in the any module section needs documentation information" $
    Checks.documentationChecksArePossible
      ( configWith
          Config.emptyForAnyModule
            { Config.anyModuleMaximumUndocumentedExports = Rules.Enforced 1
            }
          []
          []
      )
      HH.=== True

specifiedModuleRuleIsPossible :: Tasty.TestTree
specifiedModuleRuleIsPossible =
  singleProperty "A rule on a single specified module needs documentation information" $
    Checks.documentationChecksArePossible
      ( configWith
          Config.emptyForAnyModule
          []
          [(CompatGHC.mkModuleName "Only.This.Module", maximumUndocumentedExportsRule)]
      )
      HH.=== True

patternModuleRuleIsPossible :: Tasty.TestTree
patternModuleRuleIsPossible =
  singleProperty "A rule on a pattern module needs documentation information" $
    Checks.documentationChecksArePossible
      (configWith Config.emptyForAnyModule [("Only.Matching.**", maximumUndocumentedExportsRule)] [])
      HH.=== True

{- | A rule ignored where it is declared may still apply, because a module matching both a pattern
and a specified module takes its ignores from only one of the two.
-}
ignoredPatternModuleRuleIsStillPossible :: Tasty.TestTree
ignoredPatternModuleRuleIsStillPossible =
  singleProperty "A rule ignored where it is declared still needs documentation information" $
    Checks.documentationChecksArePossible
      ( configWith
          Config.emptyForAnyModule
          [
            ( "Only.Matching.**"
            , maximumUndocumentedExportsRule
                { Config.specifiedModuleRulesToIgnore = Just (Config.All True)
                }
            )
          ]
          []
      )
      HH.=== True

ruleRequiringNothingIsNotPossible :: Tasty.TestTree
ruleRequiringNothingIsNotPossible =
  singleProperty "A module header rule set to require nothing needs no documentation information" $
    Checks.documentationChecksArePossible
      ( configWith
          Config.emptyForAnyModule
            { Config.anyModuleModuleHeaderLicenseMustExistNonEmpty = Rules.Enforced False
            }
          []
          []
      )
      HH.=== False

maximumUndocumentedExportsRule :: Config.ForSpecifiedModule
maximumUndocumentedExportsRule =
  Config.emptyForSpecifiedModule
    { Config.specifiedModuleMaximumUndocumentedExports = Just (Rules.Enforced 1)
    }

configWith ::
  Config.ForAnyModule
  -> Config.ForPatternModules
  -> Config.ForSpecifiedModules
  -> Config.Config
configWith forAnyModule forPatternModules forSpecifiedModules =
  Config.Config
    { Config.forAnyModule = forAnyModule
    , Config.forPatternModules = forPatternModules
    , Config.forSpecifiedModules = forSpecifiedModules
    }
