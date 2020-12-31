module Modulint.Check
  ( CheckFailure(..)
  , QualificationViolation(..)
  , CheckedDependency(..)
  , ImportChecker
  , checkImports
  , newImportChecker
  ) where

import qualified Data.Foldable as Fold
import qualified Data.STRef as STRef
import qualified Control.Monad as Monad
import qualified Control.Monad.ST as ST

import qualified Modulint.Config as Config
import qualified Modulint.Imports as Imports
import qualified Modulint.ModuleName as ModuleName
import qualified Modulint.Qualification as Qualification
import qualified Modulint.TreeName as TreeName

data ImportChecker =
  ImportChecker
    { dependencies        :: [CheckedDependency]
    , encapsulatedTrees   :: [TreeName.TreeName]
    , qualificationRules  :: Qualification.RuleMap
    }

data CheckedDependency =
  CheckedDependency
    { dependencySource :: TreeName.TreeName
    , dependencyTarget :: TreeName.TreeName
    }

data CheckFailure
  = DependencyViolation    Imports.Import CheckedDependency
  | EncapsulationViolation Imports.Import TreeName.TreeName
  | QualificationViolation Imports.Import QualificationViolation

data QualificationViolation
  = QualificationForbidden
  | QualificationRequired [ModuleName.ModuleName]
  | DisallowedAlias ModuleName.ModuleName [ModuleName.ModuleName]

newImportChecker :: Config.Config -> ImportChecker
newImportChecker config =
  let
    explodeDependencies decl =
      map (\dep -> CheckedDependency (Config.moduleTree decl) dep)
      $ Config.treeDependencies decl

    configuredDependencies =
      concatMap explodeDependencies
      . Config.dependencyDeclarations
      $ config
  in
    ImportChecker
      { dependencies        = configuredDependencies
      , encapsulatedTrees   = Config.encapsulatedTrees config
      , qualificationRules  = Config.qualificationRules config
      }

checkImports :: Foldable f
             => ImportChecker
             -> f Imports.Import
             -> [CheckFailure]
checkImports importChecker imports =
  ST.runST $ do
    failures <- STRef.newSTRef []
    Fold.traverse_ (checkImport failures importChecker) imports
    STRef.readSTRef failures

checkImport :: STRef.STRef s [CheckFailure]
            -> ImportChecker
            -> Imports.Import
            -> ST.ST s ()
checkImport failures checker imp = do
  Fold.traverse_
    (checkImportAgainstDependency failures imp)
    (dependencies checker)

  Fold.traverse_
    (checkImportAgainstEncapsulation failures imp)
    (encapsulatedTrees checker)

  let
    targetName = ModuleName.syntaxToModuleName (Imports.importTarget imp)

  Fold.traverse_
    (checkImportQualification failures imp)
    (Qualification.lookupRule targetName (qualificationRules checker))

checkImportAgainstDependency :: STRef.STRef s [CheckFailure]
                             -> Imports.Import
                             -> CheckedDependency
                             -> ST.ST s ()
checkImportAgainstDependency failures imp dep = do
  let
    importSource = Imports.importSource imp
    importTarget = Imports.importTarget imp
    depSource    = dependencySource dep
    depTarget    = dependencyTarget dep

    -- If the file that contains the import belongs to the module tree that is
    -- the dependency target then we need to check wether the module being
    -- imported (the imported target) is contained in the module tree that the
    -- dependency is declared for. This is -- once a dependency is declared, we
    -- don't allow dependency targets to import modules that depend on them.
    dependencyViolated  =
      TreeName.treeContainsModule depTarget importSource
      && TreeName.treeContainsModule depSource importTarget

  Monad.when
    dependencyViolated
    (addFailure failures (DependencyViolation imp dep))

checkImportAgainstEncapsulation :: STRef.STRef s [CheckFailure]
                                -> Imports.Import
                                -> TreeName.TreeName
                                -> ST.ST s ()
checkImportAgainstEncapsulation failures imp encapsulatedTree = do
  let
    importSource = Imports.importSource imp
    importTarget = Imports.importTarget imp

    -- If the module being imported belongs to an encapsulated module tree
    -- then it may only be directly imported from within that tree. Imports
    -- by modules outside the encapsulated tree constitute a violation.
    encapsulationViolated =
      TreeName.treeStrictlyContainsModule encapsulatedTree importTarget
      && (not $ TreeName.treeContainsModule encapsulatedTree importSource)

  Monad.when
    encapsulationViolated
    (addFailure failures (EncapsulationViolation imp encapsulatedTree))

checkImportQualification :: STRef.STRef s [CheckFailure]
                         -> Imports.Import
                         -> Qualification.Rule
                         -> ST.ST s ()
checkImportQualification failures imp rule =
  case rule of
    Qualification.Forbidden ->
      Monad.when
        (Imports.importQualified imp)
        (addFailure failures (QualificationViolation imp QualificationForbidden))

    Qualification.RequiredAs allowedAliases -> do
      case Imports.importAlias imp of
        Nothing ->
          addFailure failures (QualificationViolation imp (QualificationRequired allowedAliases))

        Just alias -> do
          Monad.when
            (not (Imports.importQualified imp))
            (addFailure failures (QualificationViolation imp (QualificationRequired allowedAliases)))

          Monad.when
            (not (alias `elem` allowedAliases))
            (addFailure failures (QualificationViolation imp (DisallowedAlias alias allowedAliases)))

    Qualification.AllowedAs allowedAliases ->
      case Imports.importAlias imp of
        Nothing ->
          pure ()

        Just alias ->
          Monad.when
            (not (alias `elem` allowedAliases))
            (addFailure failures (QualificationViolation imp (DisallowedAlias alias allowedAliases)))


addFailure :: STRef.STRef s [CheckFailure] -> CheckFailure -> ST.ST s ()
addFailure failures err =
  STRef.modifySTRef failures (\errs -> err : errs)
