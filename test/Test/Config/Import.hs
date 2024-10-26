{-# LANGUAGE OverloadedStrings #-}

module Test.Config.Import
  ( checkImport
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified CompatGHC
import qualified Henforcer.Checks as Checks
import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Config as Config
import qualified Henforcer.Rules as Rules

singleProperty :: Tasty.TestName -> HH.PropertyT IO () -> Tasty.TestTree
singleProperty name =
  TastyHH.testProperty name . HH.withTests 1 . HH.property

checkImport :: Config.Config -> Tasty.TestTree
checkImport conf =
  Tasty.testGroup
    "Checking configuration parsing of import rules"
    [ checkAnyModule conf
    , checkPatternModule conf
    , checkPatternModulePicksFirst conf
    , checkSpecifiedModule conf
    , checkPatternModuleIgnores conf
    ]

checkAnyModule :: Config.Config -> Tasty.TestTree
checkAnyModule conf =
  singleProperty "Does folding correctly for any module section" $
    importAnyModuleRules
      HH.=== Checks.determineChecks conf (CompatGHC.mkModuleName "AnyModule")

checkPatternModule :: Config.Config -> Tasty.TestTree
checkPatternModule conf =
  singleProperty "Picks pattern module section when module matches" $
    importPatternModuleRules
      HH.=== Checks.determineChecks conf (CompatGHC.mkModuleName "Pattern.Module")

checkPatternModulePicksFirst :: Config.Config -> Tasty.TestTree
checkPatternModulePicksFirst conf =
  singleProperty "Overlapping patterns pick the first match" $
    importPatternModuleRules
      HH.=== Checks.determineChecks conf (CompatGHC.mkModuleName "Pattern2.Module")

checkSpecifiedModule :: Config.Config -> Tasty.TestTree
checkSpecifiedModule conf =
  singleProperty "Specified module is picked even if the pattern is matched" $
    importSpecifiedModuleRules
      HH.=== Checks.determineChecks
        conf
        (CompatGHC.mkModuleName "PatternMatchedButSpecifiedWins.Module")

checkPatternModuleIgnores :: Config.Config -> Tasty.TestTree
checkPatternModuleIgnores conf =
  singleProperty "Pattern module can ignore rules " $
    importPatternWithIgnoresModuleRules
      HH.=== Checks.determineChecks conf (CompatGHC.mkModuleName "WithIgnoresPattern.Module")

importAnyModuleRules :: Checks.ImportChecks
importAnyModuleRules =
  Checks.ImportChecks
    { Checks.importChecksTreeDependencies =
        Just . pure $
          Checks.CheckedDependency
            (CodeStructure.parse "Control.Monad")
            Rules.noUserNote
            (CodeStructure.parse "PetStore")
    , Checks.importChecksEncapsulatedTrees =
        Just (pure (CodeStructure.parse "Service.ThirdPartyPetsSite"))
    , Checks.importChecksAllowedQualifications =
        Map.fromList
          [
            ( CompatGHC.mkModuleName "UnliftIO"
            ,
              [ CodeStructure.SchemeWithNote
                  { CodeStructure.schemeNote = Rules.noUserNote
                  , CodeStructure.underlyingScheme =
                      CodeStructure.Scheme
                        { CodeStructure.qualification = CompatGHC.QualifiedPre
                        , CodeStructure.alias = CodeStructure.WithAlias (CompatGHC.mkModuleName "Foo")
                        , CodeStructure.safe = CodeStructure.WithoutSafe
                        , CodeStructure.packageQualification = CodeStructure.WithPackageQualifier "unliftio"
                        }
                  }
              , CodeStructure.SchemeWithNote
                  { CodeStructure.schemeNote = Rules.noUserNote
                  , CodeStructure.underlyingScheme =
                      CodeStructure.Scheme
                        { CodeStructure.qualification = CompatGHC.QualifiedPost
                        , CodeStructure.alias = CodeStructure.WithAlias (CompatGHC.mkModuleName "Foo")
                        , CodeStructure.safe = CodeStructure.WithoutSafe
                        , CodeStructure.packageQualification = CodeStructure.WithPackageQualifier "unliftio"
                        }
                  }
              ]
            )
          ]
    , Checks.importChecksAllowedOpenUnaliasedImports = Rules.Enforced 3
    , Checks.importChecksAllowedAliasUniqueness =
        Just . CodeStructure.AllAliasesUniqueExcept $
          CodeStructure.AliasUniquenessExceptions
            (Set.singleton $ CompatGHC.mkModuleName "Export")
            Rules.noUserNote
    }

importPatternModuleRules :: Checks.ImportChecks
importPatternModuleRules =
  Checks.ImportChecks
    { Checks.importChecksTreeDependencies =
        Just . pure $
          Checks.CheckedDependency
            (CodeStructure.parse "Control.Monad")
            Rules.noUserNote
            (CodeStructure.parse "PetStore")
    , Checks.importChecksEncapsulatedTrees =
        Just (pure (CodeStructure.parse "Service.ThirdPartyPetsSite"))
    , Checks.importChecksAllowedQualifications =
        Map.fromList
          [
            ( CompatGHC.mkModuleName "UnliftIO"
            ,
              [ CodeStructure.SchemeWithNote
                  { CodeStructure.schemeNote = Rules.noUserNote
                  , CodeStructure.underlyingScheme =
                      CodeStructure.Scheme
                        { CodeStructure.qualification = CompatGHC.QualifiedPre
                        , CodeStructure.alias = CodeStructure.WithAlias (CompatGHC.mkModuleName "Foo")
                        , CodeStructure.safe = CodeStructure.WithoutSafe
                        , CodeStructure.packageQualification = CodeStructure.WithPackageQualifier "unliftio"
                        }
                  }
              , CodeStructure.SchemeWithNote
                  { CodeStructure.schemeNote = Rules.noUserNote
                  , CodeStructure.underlyingScheme =
                      CodeStructure.Scheme
                        { CodeStructure.qualification = CompatGHC.QualifiedPost
                        , CodeStructure.alias = CodeStructure.WithAlias (CompatGHC.mkModuleName "Foo")
                        , CodeStructure.safe = CodeStructure.WithoutSafe
                        , CodeStructure.packageQualification = CodeStructure.WithPackageQualifier "unliftio"
                        }
                  }
              ]
            )
          ]
    , Checks.importChecksAllowedOpenUnaliasedImports = Rules.Enforced 2
    , Checks.importChecksAllowedAliasUniqueness =
        Just . CodeStructure.AllAliasesUniqueExcept $
          CodeStructure.AliasUniquenessExceptions
            (Set.singleton $ CompatGHC.mkModuleName "Export")
            Rules.noUserNote
    }

importPatternWithIgnoresModuleRules :: Checks.ImportChecks
importPatternWithIgnoresModuleRules =
  Checks.ImportChecks
    { Checks.importChecksTreeDependencies = Nothing
    , Checks.importChecksEncapsulatedTrees = Nothing
    , Checks.importChecksAllowedQualifications = mempty
    , Checks.importChecksAllowedOpenUnaliasedImports = Rules.NotEnforced
    , Checks.importChecksAllowedAliasUniqueness = Nothing
    }

importSpecifiedModuleRules :: Checks.ImportChecks
importSpecifiedModuleRules =
  Checks.ImportChecks
    { Checks.importChecksTreeDependencies =
        Just . pure $
          Checks.CheckedDependency
            (CodeStructure.parse "Control.Monad")
            Rules.noUserNote
            (CodeStructure.parse "PetStore")
    , Checks.importChecksEncapsulatedTrees =
        Just
          (pure (CodeStructure.parse "Service.ThirdPartyPetsSite"))
    , Checks.importChecksAllowedQualifications =
        Map.fromList
          [
            ( CompatGHC.mkModuleName "UnliftIO"
            ,
              [ CodeStructure.SchemeWithNote
                  { CodeStructure.schemeNote = Rules.noUserNote
                  , CodeStructure.underlyingScheme =
                      CodeStructure.Scheme
                        { CodeStructure.qualification = CompatGHC.QualifiedPre
                        , CodeStructure.alias = CodeStructure.WithAlias (CompatGHC.mkModuleName "Foo")
                        , CodeStructure.safe = CodeStructure.WithoutSafe
                        , CodeStructure.packageQualification = CodeStructure.WithPackageQualifier "unliftio"
                        }
                  }
              , CodeStructure.SchemeWithNote
                  { CodeStructure.schemeNote = Rules.noUserNote
                  , CodeStructure.underlyingScheme =
                      CodeStructure.Scheme
                        { CodeStructure.qualification = CompatGHC.QualifiedPost
                        , CodeStructure.alias = CodeStructure.WithAlias (CompatGHC.mkModuleName "Foo")
                        , CodeStructure.safe = CodeStructure.WithoutSafe
                        , CodeStructure.packageQualification = CodeStructure.WithPackageQualifier "unliftio"
                        }
                  }
              ]
            )
          ]
    , Checks.importChecksAllowedOpenUnaliasedImports = Rules.Enforced 20
    , Checks.importChecksAllowedAliasUniqueness =
        Just . CodeStructure.AllAliasesUniqueExcept $
          CodeStructure.AliasUniquenessExceptions
            (Set.singleton $ CompatGHC.mkModuleName "Export")
            Rules.noUserNote
    }
