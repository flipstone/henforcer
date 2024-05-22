{- |
Module      : Henforcer.Config.ForSpecifiedModule
Description : Functionality for specifying rules scoped to a particular module.
Copyright   : (c) Flipstone Technology Partners, 2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config.ForSpecifiedModule
  ( ForSpecifiedModule (..)
  , emptyForSpecifiedModule
  , forSpecifiedModuleCodec
  , specifiedModuleMapCodecField
  , SpecifiedModuleMap
  ) where

import qualified Data.Map.Strict as M
import qualified Toml

import qualified CompatGHC
import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Config.IgnoreRules as IgnoreRules
import qualified Henforcer.Rules as Rules
import qualified TomlHelper

data ForSpecifiedModule = ForSpecifiedModule
  { specifiedModuleAllowedQualifications :: !(Maybe CodeStructure.AllowedSchemes)
  , specifiedModuleAllowedOpenUnaliasedImports :: !(Maybe Rules.MaximumAllowed)
  , specifiedModuleAllowedAliasUniqueness :: !(Maybe CodeStructure.AllowedAliasUniqueness)
  , specifiedModuleMaximumUndocumentedExports :: !(Maybe Rules.MaximumAllowed)
  , specifiedModuleMinimumDocumentedExports :: !(Maybe Rules.MinimumAllowed)
  , specifiedModuleMaximumExportsWithoutSince :: !(Maybe Rules.MaximumAllowed)
  , specifiedModuleMinimumExportsWithSince :: !(Maybe Rules.MinimumAllowed)
  , specifiedModuleModuleHeaderCopyrightMustExistNonEmpty :: !(Maybe Rules.MustExistNonEmpty)
  , specifiedModuleModuleHeaderDescriptionMustExistNonEmpty :: !(Maybe Rules.MustExistNonEmpty)
  , specifiedModuleModuleHeaderLicenseMustExistNonEmpty :: !(Maybe Rules.MustExistNonEmpty)
  , specifiedModuleModuleHeaderMaintainerMustExistNonEmpty :: !(Maybe Rules.MustExistNonEmpty)
  , specifiedModuleRulesToIgnore :: !(Maybe IgnoreRules.RulesToIgnore)
  }

emptyForSpecifiedModule :: ForSpecifiedModule
emptyForSpecifiedModule =
  ForSpecifiedModule
    { specifiedModuleAllowedQualifications = Nothing
    , specifiedModuleAllowedOpenUnaliasedImports = Nothing
    , specifiedModuleAllowedAliasUniqueness = Nothing
    , specifiedModuleMaximumUndocumentedExports = Nothing
    , specifiedModuleMinimumDocumentedExports = Nothing
    , specifiedModuleMaximumExportsWithoutSince = Nothing
    , specifiedModuleMinimumExportsWithSince = Nothing
    , specifiedModuleModuleHeaderCopyrightMustExistNonEmpty = Nothing
    , specifiedModuleModuleHeaderDescriptionMustExistNonEmpty = Nothing
    , specifiedModuleModuleHeaderLicenseMustExistNonEmpty = Nothing
    , specifiedModuleModuleHeaderMaintainerMustExistNonEmpty = Nothing
    , specifiedModuleRulesToIgnore = Nothing
    }

forSpecifiedModuleCodec :: Toml.TomlCodec ForSpecifiedModule
forSpecifiedModuleCodec =
  ForSpecifiedModule
    <$> TomlHelper.addField
      "allowedQualifications"
      specifiedModuleAllowedQualifications
      (Toml.dioptional . CodeStructure.allowedSchemesCodec)
    <*> TomlHelper.addField
      "allowedOpenUnaliasedImports"
      specifiedModuleAllowedOpenUnaliasedImports
      (Toml.dioptional . Rules.maximumAllowedCodec)
    <*> TomlHelper.addField
      "allowedAliasUniqueness"
      specifiedModuleAllowedAliasUniqueness
      (Toml.dioptional . CodeStructure.allowedAliasUniquenessCodec)
    <*> TomlHelper.addField
      "maximumExportsPlusHeaderUndocumented"
      specifiedModuleMaximumUndocumentedExports
      (Toml.dioptional . Rules.maximumAllowedCodec)
    <*> TomlHelper.addField
      "minimumExportsPlusHeaderDocumented"
      specifiedModuleMinimumDocumentedExports
      (Toml.dioptional . Rules.minimumAllowedCodec)
    <*> TomlHelper.addField
      "maximumExportsWithoutSince"
      specifiedModuleMaximumExportsWithoutSince
      (Toml.dioptional . Rules.maximumAllowedCodec)
    <*> TomlHelper.addField
      "minimumExportsWithSince"
      specifiedModuleMinimumExportsWithSince
      (Toml.dioptional . Rules.minimumAllowedCodec)
    <*> TomlHelper.addField
      "moduleHeaderCopyrightMustExistNonEmpty"
      specifiedModuleModuleHeaderCopyrightMustExistNonEmpty
      (Toml.dioptional . Rules.mustExistNonEmptyCodec)
    <*> TomlHelper.addField
      "moduleHeaderDescriptionMustExistNonEmpty"
      specifiedModuleModuleHeaderDescriptionMustExistNonEmpty
      (Toml.dioptional . Rules.mustExistNonEmptyCodec)
    <*> TomlHelper.addField
      "moduleHeaderLicenseMustExistNonEmpty"
      specifiedModuleModuleHeaderLicenseMustExistNonEmpty
      (Toml.dioptional . Rules.mustExistNonEmptyCodec)
    <*> TomlHelper.addField
      "moduleHeaderMaintainerMustExistNonEmpty"
      specifiedModuleModuleHeaderMaintainerMustExistNonEmpty
      (Toml.dioptional . Rules.mustExistNonEmptyCodec)
    <*> TomlHelper.addField
      "rulesToIgnore"
      specifiedModuleRulesToIgnore
      (Toml.dioptional . (Toml.table IgnoreRules.rulestoIgnoreCodec))

type SpecifiedModuleMap = M.Map CompatGHC.ModuleName ForSpecifiedModule

specifiedModuleMapCodecField ::
  Toml.Key
  -> Toml.TomlCodec (M.Map CompatGHC.ModuleName ForSpecifiedModule)
specifiedModuleMapCodecField = CompatGHC.moduleNameMapCodec forSpecifiedModuleCodec
