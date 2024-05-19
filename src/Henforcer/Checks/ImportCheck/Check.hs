{- |
Module      : Henforcer.Checks.ImportCheck.Check
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Checks.ImportCheck.Check
  ( checkImports
  , determineChecks
  ) where

import qualified Control.Monad as Monad
import qualified Data.DList as DList
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
  , CheckFailureWithNote
  , CheckedDependency (CheckedDependency, dependencyNote, dependencySource, dependencyTarget)
  )
import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Config as Config
import qualified Henforcer.Rules as Rules

checkImportAgainstDependency ::
  CodeStructure.Import
  -> CheckedDependency
  -> [CheckFailureWithNote]
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
        then pure . Rules.failureWithUserNote (dependencyNote dep) $ DependencyViolation imp dep
        else mempty

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
      else mempty

data ImportChecks = ImportChecks
  { importChecksDependencyDeclarations :: !(Maybe (NEL.NonEmpty CheckedDependency)) -- Nothing means we won't even check
  , importChecksEncapsulatedTrees :: !(Maybe (NEL.NonEmpty CodeStructure.TreeName)) -- Nothing means we won't even check
  , importChecksAllowedQualifications :: !CodeStructure.AllowedSchemes
  , importChecksAllowedOpenUnaliasedImports :: !Rules.MaximumAllowed
  , importChecksAllowedAliasUniqueness :: !CodeStructure.AllowedAliasUniqueness
  }

determineChecks :: Config.Config -> CompatGHC.ModuleName -> ImportChecks
determineChecks config modName =
  determineChecksForModule
    (Config.forAnyModule config)
    . maybe Config.emptyForSpecifiedModule id
    . Map.lookup modName
    $ Config.forSpecifiedModules config

determineChecksForModule ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> ImportChecks
determineChecksForModule fam fsm =
  let
    explodeDependencies decl =
      fmap (CheckedDependency (Config.moduleTree decl) (Config.dependencyDeclarationNote decl)) $
        Config.treeDependencies decl
   in
    ImportChecks
      { importChecksDependencyDeclarations =
          NEL.nonEmpty . concatMap explodeDependencies $ Config.anyModuleDependencyDeclarations fam
      , importChecksEncapsulatedTrees = NEL.nonEmpty $ Config.anyModuleEncapsulatedTrees fam
      , importChecksAllowedQualifications =
          case Config.specifiedModuleAllowedQualifications fsm of
            Nothing -> Config.anyModuleAllowedQualifications fam
            Just quals -> Map.union quals $ Config.anyModuleAllowedQualifications fam
      , importChecksAllowedOpenUnaliasedImports =
          Maybe.fromMaybe
            (Config.anyModuleAllowedOpenUnaliasedImports fam)
            (Config.specifiedModuleAllowedOpenUnaliasedImports fsm)
      , importChecksAllowedAliasUniqueness =
          Maybe.fromMaybe
            (Config.anyModuleAllowedAliasUniqueness fam)
            (Config.specifiedModuleAllowedAliasUniqueness fsm)
      }

data ImportCheckAccum = ImportCheckAccum
  { failures :: !(DList.DList CheckFailureWithNote)
  , openUnaliasedImports :: !(DList.DList CodeStructure.Import)
  , aliasedImportsByAliasName :: !(Map.Map CompatGHC.ModuleName [CodeStructure.Import])
  }

checkImports ::
  ImportChecks
  -> CompatGHC.TcGblEnv
  -> [CheckFailureWithNote]
checkImports checks tcGblEnv =
  let
    imports = CodeStructure.getImports tcGblEnv
    accumulatedResults = foldr (checkImport checks) (ImportCheckAccum mempty mempty mempty) imports
    openUnaliasedResults =
      fmap Rules.failureWithNoNote
        . Maybe.catMaybes
        $ Rules.checkMaximum
          (importChecksAllowedOpenUnaliasedImports checks)
          (openUnaliasedImports accumulatedResults)
          (fromIntegral . length . DList.toList)
          (\dlist nat -> fmap (flip OpenImportViolation nat) . NEL.nonEmpty $ DList.toList dlist)
    aliasUniquenessResults =
      checkAllowedAliasUniquness
        (importChecksAllowedAliasUniqueness checks)
        (aliasedImportsByAliasName accumulatedResults)
   in
    DList.toList (failures accumulatedResults) <> openUnaliasedResults <> aliasUniquenessResults

checkImport ::
  ImportChecks
  -> CodeStructure.Import
  -> ImportCheckAccum
  -> ImportCheckAccum
checkImport checks imp =
  encapsulationCheck
    checks
    imp
    . dependencyCheck
      checks
      imp
    . importQualificationCheck
      checks
      imp
    . mbAddOpenImport
      checks
      imp
    . addToAliasMap
      checks
      imp

encapsulationCheck ::
  ImportChecks
  -> CodeStructure.Import
  -> ImportCheckAccum
  -> ImportCheckAccum
encapsulationCheck checks imp accum =
  case importChecksEncapsulatedTrees checks of
    Nothing ->
      accum
    Just nonEmptyTrees ->
      accum
        { failures =
            DList.append (failures accum)
              . DList.fromList
              . concatMap (fmap Rules.failureWithNoNote . checkImportAgainstEncapsulation imp)
              $ NEL.toList nonEmptyTrees
        }

dependencyCheck ::
  ImportChecks
  -> CodeStructure.Import
  -> ImportCheckAccum
  -> ImportCheckAccum
dependencyCheck checks imp accum =
  case importChecksDependencyDeclarations checks of
    Nothing ->
      accum
    Just nonEmptyDependencies ->
      accum
        { failures =
            DList.append (failures accum)
              . DList.fromList
              . Monad.join
              . NEL.toList
              $ fmap (checkImportAgainstDependency imp) nonEmptyDependencies
        }

importQualificationCheck ::
  ImportChecks
  -> CodeStructure.Import
  -> ImportCheckAccum
  -> ImportCheckAccum
importQualificationCheck checks imp accum =
  case Map.lookup (CodeStructure.importedModule imp) (importChecksAllowedQualifications checks) of
    Nothing ->
      accum
    Just allowedSchemes ->
      let
        (schemes, notes) =
          unzip $
            fmap (\note -> (CodeStructure.underlyingScheme note, CodeStructure.schemeNote note)) allowedSchemes
        withNotes :: CheckFailure -> CheckFailureWithNote
        withNotes = Rules.failureWithUserNotes notes
       in
        if elem
          ( CodeStructure.keepOnlyPackageNameInQualifier . CodeStructure.buildScheme . CompatGHC.unLoc $
              CodeStructure.importDecl imp
          )
          schemes
          then accum
          else
            accum
              { failures =
                  DList.cons (withNotes (QualificationViolation imp schemes)) (failures accum)
              }

mbAddOpenImport ::
  ImportChecks
  -> CodeStructure.Import
  -> ImportCheckAccum
  -> ImportCheckAccum
mbAddOpenImport checks imp accum =
  case importChecksAllowedOpenUnaliasedImports checks of
    Rules.NoMaximumToEnforce ->
      accum
    Rules.MaximumAllowed _ ->
      if CodeStructure.importIsOpenWithNoHidingOrAlias imp
        then
          accum
            { openUnaliasedImports =
                DList.cons imp $ openUnaliasedImports accum
            }
        else accum

addToAliasMap ::
  ImportChecks
  -> CodeStructure.Import
  -> ImportCheckAccum
  -> ImportCheckAccum
addToAliasMap checks imp accum =
  case importChecksAllowedAliasUniqueness checks of
    CodeStructure.NoAliasUniqueness ->
      accum
    _ ->
      case CodeStructure.alias . CodeStructure.buildScheme . CompatGHC.unLoc $ CodeStructure.importDecl imp of
        CodeStructure.WithAlias modName ->
          accum
            { aliasedImportsByAliasName =
                Map.insertWith (<>) modName (pure imp) (aliasedImportsByAliasName accum)
            }
        CodeStructure.WithoutAlias ->
          accum

checkAllowedAliasUniquness ::
  CodeStructure.AllowedAliasUniqueness
  -> Map.Map CompatGHC.ModuleName [CodeStructure.Import]
  -> [CheckFailureWithNote]
checkAllowedAliasUniquness allowedUniqueness aliasedImports =
  let
    buildUniquenessViolation ::
      Rules.UserNote -> Map.Map k [CodeStructure.Import] -> [CheckFailureWithNote]
    buildUniquenessViolation note =
      fmap (Rules.failureWithUserNote note . AliasUniquenessViolation)
        . Maybe.mapMaybe NEL.nonEmpty
        . Map.elems
        . Map.filter ((<) 1 . length)
   in
    case allowedUniqueness of
      CodeStructure.NoAliasUniqueness ->
        mempty
      CodeStructure.AliasesToBeUnique modNames userNote ->
        buildUniquenessViolation userNote $ Map.restrictKeys aliasedImports modNames
      CodeStructure.AllAliasesUniqueExcept modNames userNote ->
        buildUniquenessViolation userNote $ Map.withoutKeys aliasedImports modNames
