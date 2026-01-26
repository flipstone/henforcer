{- |
Module      : Henforcer.Config
Description : Functionaly around specifying rules that apply to any given module.
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config.ForAnyModule
  ( ForAnyModule (..)
  , forAnyModuleCodec
  , emptyForAnyModule
  ) where

import qualified Toml

import qualified Henforcer.CodeStructure as CodeStructure
import Henforcer.Config.TreeDependencies (TreeDependency, treeDependencyCodec)
import qualified Henforcer.Rules as Rules
import qualified TomlHelper

data ForAnyModule = ForAnyModule
  { anyModuleTreeDependencies :: ![TreeDependency]
  , anyModuleEncapsulatedTrees :: ![CodeStructure.ModuleTree]
  , anyModuleAllowedQualifications :: !CodeStructure.AllowedSchemes
  , anyModuleAllowedOpenUnaliasedImports :: !Rules.MaximumAllowed
  , anyModuleAllowedAliasUniqueness :: !(Maybe CodeStructure.AllowedAliasUniqueness)
  , anyModuleMaximumUndocumentedExports :: !Rules.MaximumAllowed
  , anyModuleMinimumDocumentedExports :: !Rules.MinimumAllowed
  , anyModuleMaximumExportsWithoutSince :: !Rules.MaximumAllowed
  , anyModuleMinimumExportsWithSince :: !Rules.MinimumAllowed
  , anyModuleModuleHeaderCopyrightMustExistNonEmpty :: !Rules.MustExistNonEmpty
  , anyModuleModuleHeaderDescriptionMustExistNonEmpty :: !Rules.MustExistNonEmpty
  , anyModuleModuleHeaderLicenseMustExistNonEmpty :: !Rules.MustExistNonEmpty
  , anyModuleModuleHeaderMaintainerMustExistNonEmpty :: !Rules.MustExistNonEmpty
  }

forAnyModuleCodec :: Toml.TomlCodec ForAnyModule
forAnyModuleCodec =
  ForAnyModule
    <$> TomlHelper.addField
      "treeDependencies"
      anyModuleTreeDependencies
      (Toml.list treeDependencyCodec)
    <*> TomlHelper.addField
      "encapsulatedTrees"
      anyModuleEncapsulatedTrees
      (TomlHelper.setDefault [] CodeStructure.moduleTreeListCodec)
    <*> TomlHelper.addField
      "allowedQualifications"
      anyModuleAllowedQualifications
      CodeStructure.allowedSchemesCodec
    <*> TomlHelper.addField
      "allowedOpenUnaliasedImports"
      anyModuleAllowedOpenUnaliasedImports
      Rules.maximumAllowedCodec
    <*> TomlHelper.addField
      "allowedAliasUniqueness"
      anyModuleAllowedAliasUniqueness
      (Toml.dioptional . Toml.table CodeStructure.allowedAliasUniquenessCodec)
    <*> TomlHelper.addField
      "maximumExportsPlusHeaderUndocumented"
      anyModuleMaximumUndocumentedExports
      Rules.maximumAllowedCodec
    <*> TomlHelper.addField
      "minimumExportsPlusHeaderDocumented"
      anyModuleMinimumDocumentedExports
      Rules.minimumAllowedCodec
    <*> TomlHelper.addField
      "maximumExportsWithoutSince"
      anyModuleMaximumExportsWithoutSince
      Rules.maximumAllowedCodec
    <*> TomlHelper.addField
      "minimumExportsWithSince"
      anyModuleMinimumExportsWithSince
      Rules.minimumAllowedCodec
    <*> TomlHelper.addField
      "moduleHeaderCopyrightMustExistNonEmpty"
      anyModuleModuleHeaderCopyrightMustExistNonEmpty
      Rules.mustExistNonEmptyCodec
    <*> TomlHelper.addField
      "moduleHeaderDescriptionMustExistNonEmpty"
      anyModuleModuleHeaderDescriptionMustExistNonEmpty
      Rules.mustExistNonEmptyCodec
    <*> TomlHelper.addField
      "moduleHeaderLicenseMustExistNonEmpty"
      anyModuleModuleHeaderLicenseMustExistNonEmpty
      Rules.mustExistNonEmptyCodec
    <*> TomlHelper.addField
      "moduleHeaderMaintainerMustExistNonEmpty"
      anyModuleModuleHeaderMaintainerMustExistNonEmpty
      Rules.mustExistNonEmptyCodec

emptyForAnyModule :: ForAnyModule
emptyForAnyModule =
  ForAnyModule
    { anyModuleTreeDependencies = mempty
    , anyModuleEncapsulatedTrees = mempty
    , anyModuleAllowedQualifications = mempty
    , anyModuleAllowedOpenUnaliasedImports = Rules.NotEnforced
    , anyModuleAllowedAliasUniqueness = Nothing
    , anyModuleMaximumUndocumentedExports = Rules.NotEnforced
    , anyModuleMinimumDocumentedExports = Rules.NotEnforced
    , anyModuleMaximumExportsWithoutSince = Rules.NotEnforced
    , anyModuleMinimumExportsWithSince = Rules.NotEnforced
    , anyModuleModuleHeaderCopyrightMustExistNonEmpty = Rules.NotEnforced
    , anyModuleModuleHeaderDescriptionMustExistNonEmpty = Rules.NotEnforced
    , anyModuleModuleHeaderLicenseMustExistNonEmpty = Rules.NotEnforced
    , anyModuleModuleHeaderMaintainerMustExistNonEmpty = Rules.NotEnforced
    }
