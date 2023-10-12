{- |
Module      : Henforcer.Checks.ImportCheck.Check
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Checks.ImportCheck.Check
  ( ImportChecker
  , newImportChecker
  , checkModule
  ) where

import qualified Control.Monad as Monad
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified CompatGHC
import Henforcer.Checks.ImportCheck.CheckFailure
  ( CheckFailure
      ( AliasUniquenessViolation
      , DependencyViolation
      , EncapsulationViolation
      , OpenImportViolation
      , QualificationViolation
      )
  , CheckedDependency (CheckedDependency, dependencySource, dependencyTarget)
  )
import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Config as Config

data ImportChecker = ImportChecker
  { dependencies :: ![CheckedDependency]
  , encapsulatedTrees :: ![CodeStructure.TreeName]
  , allowedQualifications :: !CodeStructure.AllowedSchemes
  , defaultAllowedOpenUnaliasedImports :: !CodeStructure.DefaultAllowedOpenUnaliasedImports
  , perModuleOpenUnaliasedImports :: !CodeStructure.PerModuleAllowedOpenUnaliasedImports
  , allowedAliasUniqueness :: !CodeStructure.AllowedAliasUniqueness
  }

checkModule :: ImportChecker -> CompatGHC.TcGblEnv -> [CheckFailure]
checkModule importChecker tcGblEnv =
  let imports = CodeStructure.getImports tcGblEnv
      name = CompatGHC.moduleName $ CompatGHC.tcg_mod tcGblEnv
      importFailures = Monad.join $ fmap (checkImport importChecker) imports
      openImportFailures =
        checkOpenImport
          name
          (defaultAllowedOpenUnaliasedImports importChecker)
          (perModuleOpenUnaliasedImports importChecker)
          imports
      aliasUniquenessFailures =
        checkAllowedAliasUniquness (allowedAliasUniqueness importChecker) imports
   in importFailures <> openImportFailures <> aliasUniquenessFailures

newImportChecker :: Config.Config -> ImportChecker
newImportChecker config =
  let explodeDependencies decl =
        fmap (CheckedDependency (Config.moduleTree decl)) $
          Config.treeDependencies decl

      configuredDependencies =
        concatMap explodeDependencies $
          Config.dependencyDeclarations config
   in ImportChecker
        { dependencies = configuredDependencies
        , encapsulatedTrees = Config.encapsulatedTrees config
        , allowedQualifications = Config.allowedQualifications config
        , defaultAllowedOpenUnaliasedImports = Config.defaultAllowedOpenUnaliasedImports config
        , perModuleOpenUnaliasedImports = Config.perModuleOpenUnaliasedImports config
        , allowedAliasUniqueness = Config.allowedAliasUniqueness config
        }

checkImport ::
  ImportChecker
  -> CodeStructure.Import
  -> [CheckFailure]
checkImport checker imp =
  let targetName = CodeStructure.importedModule imp
      dependencyFailures = Monad.join $ fmap (checkImportAgainstDependency imp) (dependencies checker)
      encapsulationFailures = Monad.join $ fmap (checkImportAgainstEncapsulation imp) (encapsulatedTrees checker)
      qualificationFailures =
        maybe [] (checkImportQualification imp) $
          Map.lookup targetName (allowedQualifications checker)
   in dependencyFailures <> encapsulationFailures <> qualificationFailures

checkImportAgainstDependency ::
  CodeStructure.Import
  -> CheckedDependency
  -> [CheckFailure]
checkImportAgainstDependency imp dep =
  let depSource = dependencySource dep
      depTarget = dependencyTarget dep

      -- If the file that contains the import belongs to the module tree that is
      -- the dependency target then we need to check wether the module being
      -- imported (the imported target) is contained in the module tree that the
      -- dependency is declared for. This is -- once a dependency is declared, we
      -- don't allow dependency targets to import modules that depend on them.
      dependencyViolated =
        CodeStructure.treeContainsModule depTarget (CodeStructure.srcModule imp)
          && CodeStructure.treeContainsModule depSource (CodeStructure.importedModule imp)
   in if dependencyViolated
        then pure $ DependencyViolation imp dep
        else []

checkImportAgainstEncapsulation ::
  CodeStructure.Import
  -> CodeStructure.TreeName
  -> [CheckFailure]
checkImportAgainstEncapsulation imp encapsulatedTree =
  let
    -- If the module being imported belongs to an encapsulated module tree
    -- then it may only be directly imported from within that tree. Imports
    -- by modules outside the encapsulated tree constitute a violation.
    encapsulationViolated =
      CodeStructure.treeStrictlyContainsModule encapsulatedTree (CodeStructure.importedModule imp)
        && (not . CodeStructure.treeContainsModule encapsulatedTree $ CodeStructure.srcModule imp)
   in
    if encapsulationViolated
      then pure $ EncapsulationViolation imp encapsulatedTree
      else []

checkImportQualification ::
  CodeStructure.Import
  -> [CodeStructure.Scheme]
  -> [CheckFailure]
checkImportQualification imp alloweds =
  if elem
    ( CodeStructure.keepOnlyPackageNameInQualifier . CodeStructure.buildScheme . CompatGHC.unLoc $
        CodeStructure.importDecl imp
    )
    alloweds
    then []
    else pure $ QualificationViolation imp alloweds

checkOpenImport ::
  CompatGHC.ModuleName
  -> CodeStructure.DefaultAllowedOpenUnaliasedImports
  -> CodeStructure.PerModuleAllowedOpenUnaliasedImports
  -> [CodeStructure.Import]
  -> [CheckFailure]
checkOpenImport modName defaultMax perModuleMax imports =
  let openImports = filter CodeStructure.importIsOpenWithNoHidingOrAlias imports
   in case (openImports, CodeStructure.determineMaxAllowedForModule defaultMax perModuleMax modName) of
        ([], _) ->
          []
        (_, CodeStructure.NoMaxToEnforce) ->
          []
        (x : xs, CodeStructure.MaxForModule nat) ->
          checkNonEmptyOpenImports (x NEL.:| xs) nat

checkNonEmptyOpenImports ::
  NEL.NonEmpty CodeStructure.Import
  -> CodeStructure.MaxOpenUnaliasedImportsNat
  -> [CheckFailure]
checkNonEmptyOpenImports nonEmptyOpenImports nat = do
  if NEL.length nonEmptyOpenImports > fromIntegral nat
    then pure (OpenImportViolation nonEmptyOpenImports nat)
    else []

checkAllowedAliasUniquness ::
  (Foldable t) =>
  CodeStructure.AllowedAliasUniqueness
  -> t CodeStructure.Import
  -> [CheckFailure]
checkAllowedAliasUniquness aliasUniqueness imports =
  let
    keepAliasedImportByModName accum i =
      case CodeStructure.alias . CodeStructure.buildScheme . CompatGHC.unLoc $ CodeStructure.importDecl i of
        CodeStructure.WithAlias modName ->
          (modName, [i]) : accum
        CodeStructure.WithoutAlias -> accum

    aliasedSchemes :: Map.Map CompatGHC.ModuleName [CodeStructure.Import]
    aliasedSchemes =
      Map.fromListWith (<>) $ L.foldl' keepAliasedImportByModName [] imports

    buildUniquenessViolation :: Map.Map k [CodeStructure.Import] -> [CheckFailure]
    buildUniquenessViolation =
      fmap AliasUniquenessViolation
        . Maybe.mapMaybe NEL.nonEmpty
        . Map.elems
        . Map.filter ((<) 1 . length)

    handleExcepts =
      buildUniquenessViolation . Map.withoutKeys aliasedSchemes

    handleUniques =
      buildUniquenessViolation . Map.restrictKeys aliasedSchemes
   in
    -- filter imports that aliased with the given set
    -- group by alias name, keep results length >1
    --      filter ((<) 1 . length) . L.groupBy (\x y -> (CodeStructure.alias x) == (CodeStructure.alias y)) $ filter (CodeStructure.isAliasIn aliasesRequiredToBeUnique) aliasedSchemes

    case aliasUniqueness of
      CodeStructure.AliasesToBeUnique modNames ->
        handleUniques modNames
      CodeStructure.AllAliasesUniqueExcept modNames ->
        handleExcepts modNames
      CodeStructure.NoAliasUniqueness ->
        []
