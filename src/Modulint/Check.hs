module Modulint.Check
  ( CheckFailure(..)
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
import qualified Modulint.Qualification as Qualification
import qualified Modulint.TreeName as TreeName

data ImportChecker =
  ImportChecker
    { dependencies           :: [CheckedDependency]
    , encapsulatedTrees      :: [TreeName.TreeName]
    , allowedQualifications  :: Qualification.AllowedMap
    }

data CheckedDependency =
  CheckedDependency
    { dependencySource :: TreeName.TreeName
    , dependencyTarget :: TreeName.TreeName
    }

data CheckFailure
  = DependencyViolation    Imports.Import CheckedDependency
  | EncapsulationViolation Imports.Import TreeName.TreeName
  | QualificationViolation Imports.Import [Qualification.AllowedQualification]


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
      { dependencies          = configuredDependencies
      , encapsulatedTrees     = Config.encapsulatedTrees config
      , allowedQualifications = Config.allowedQualifications config
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
    targetName = Imports.importedModule imp

  Fold.traverse_
    (checkImportQualification failures imp)
    (Qualification.lookupAllowed targetName (allowedQualifications checker))

checkImportAgainstDependency :: STRef.STRef s [CheckFailure]
                             -> Imports.Import
                             -> CheckedDependency
                             -> ST.ST s ()
checkImportAgainstDependency failures imp dep = do
  let
    srcModule       = Imports.srcModule imp
    importedModule  = Imports.importedModule imp
    depSource       = dependencySource dep
    depTarget       = dependencyTarget dep

    -- If the file that contains the import belongs to the module tree that is
    -- the dependency target then we need to check wether the module being
    -- imported (the imported target) is contained in the module tree that the
    -- dependency is declared for. This is -- once a dependency is declared, we
    -- don't allow dependency targets to import modules that depend on them.
    dependencyViolated  =
      TreeName.treeContainsModule depTarget srcModule
      && TreeName.treeContainsModule depSource importedModule

  Monad.when
    dependencyViolated
    (addFailure failures (DependencyViolation imp dep))

checkImportAgainstEncapsulation :: STRef.STRef s [CheckFailure]
                                -> Imports.Import
                                -> TreeName.TreeName
                                -> ST.ST s ()
checkImportAgainstEncapsulation failures imp encapsulatedTree = do
  let
    srcModule       = Imports.srcModule imp
    importedModule  = Imports.importedModule imp

    -- If the module being imported belongs to an encapsulated module tree
    -- then it may only be directly imported from within that tree. Imports
    -- by modules outside the encapsulated tree constitute a violation.
    encapsulationViolated =
      TreeName.treeStrictlyContainsModule encapsulatedTree importedModule
      && (not $ TreeName.treeContainsModule encapsulatedTree srcModule)

  Monad.when
    encapsulationViolated
    (addFailure failures (EncapsulationViolation imp encapsulatedTree))

checkImportQualification :: STRef.STRef s [CheckFailure]
                         -> Imports.Import
                         -> [Qualification.AllowedQualification]
                         -> ST.ST s ()
checkImportQualification failures imp alloweds =
  if
    any (isAllowedBy imp) alloweds
  then
    pure ()
  else
    addFailure failures (QualificationViolation imp alloweds)

isAllowedBy :: Imports.Import -> Qualification.AllowedQualification -> Bool
isAllowedBy imp allowed =
  qualifiedCorrectly && aliasedCorrectly
    where
      qualifiedCorrectly =
        case Qualification.qualification allowed of
          Qualification.Qualified ->
            Imports.isQualified imp

          Qualification.Unqualified ->
            not (Imports.isQualified imp)

      aliasedCorrectly =
        case (Imports.alias imp, Qualification.alias allowed) of
          (Nothing, Qualification.WithoutAlias) ->
            True

          (Just impAlias, Qualification.WithAlias allowedAlias) ->
            impAlias == allowedAlias

          _ ->
            False



addFailure :: STRef.STRef s [CheckFailure] -> CheckFailure -> ST.ST s ()
addFailure failures err =
  STRef.modifySTRef failures (\errs -> err : errs)
