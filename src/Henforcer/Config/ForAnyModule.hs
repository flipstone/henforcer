{- |
Module      : Henforcer.Config
Description : Functionaly around specifying rules that apply to any given module.
Copyright   : (c) Flipstone Technology Partners, 2023-2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config.ForAnyModule
  ( ForAnyModule (..)
  , forAnyModuleCodec
  ) where

import qualified Toml

import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Rules as Rules
import qualified TomlHelper

import Henforcer.Config.TreeDependencies (TreeDependency, treeDependencyCodec)

data ForAnyModule = ForAnyModule
  { anyModuleTreeDependencies :: ![TreeDependency]
  , anyModuleEncapsulatedTrees :: ![CodeStructure.TreeName]
  , anyModuleAllowedQualifications :: !CodeStructure.AllowedSchemes
  , anyModuleAllowedOpenUnaliasedImports :: !Rules.MaximumAllowed
  , anyModuleAllowedAliasUniqueness :: !CodeStructure.AllowedAliasUniqueness

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
    <*> TomlHelper.addField "encapsulatedTrees" anyModuleEncapsulatedTrees CodeStructure.treeNameListCodec
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
      CodeStructure.allowedAliasUniquenessCodec
    <*> TomlHelper.addField
      "maximumExportsPlusHeaderUndocumented"
      anyModuleAllowedOpenUnaliasedImports
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
