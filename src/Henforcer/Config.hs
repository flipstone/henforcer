{- |
Module      : Henforcer.Config
Description : Haskell representation of the toml configuration file that is used to define the checks to be enforced.
Copyright   : (c) Flipstone Technology Partners, 2023-2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config
  ( DependencyDeclaration (..)
  , Config (..)
  , loadConfigFileWithFingerprint
  , ForAnyModule (..)
  , ForSpecifiedModule (..)
  , emptyForSpecifiedModule
  , configCodec
  ) where

import qualified Data.Map.Strict as M
import qualified Toml

import qualified CompatGHC
import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Rules as Rules
import qualified TomlHelper

data DependencyDeclaration = DependencyDeclaration
  { moduleTree :: CodeStructure.TreeName
  , treeDependencies :: [CodeStructure.TreeName]
  , dependencyDeclarationNote :: Rules.UserNote
  }

dependencyDeclarationCodec :: Toml.TomlCodec DependencyDeclaration
dependencyDeclarationCodec =
  DependencyDeclaration
    <$> TomlHelper.addField "moduleTree" moduleTree CodeStructure.treeNameCodec
    <*> TomlHelper.addField "dependencies" treeDependencies CodeStructure.treeNameListCodec
    <*> Rules.userNoteField dependencyDeclarationNote

data ForAnyModule = ForAnyModule
  { anyModuleDependencyDeclarations :: ![DependencyDeclaration]
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
      anyModuleDependencyDeclarations
      (Toml.list dependencyDeclarationCodec)
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

type SpecifiedModuleMap = M.Map CompatGHC.ModuleName ForSpecifiedModule

data Config = Config
  { forAnyModule :: !ForAnyModule
  , forSpecifiedModules :: !SpecifiedModuleMap
  }

configCodec :: Toml.TomlCodec Config
configCodec =
  Config
    <$> TomlHelper.addField "forAnyModule" forAnyModule (Toml.table forAnyModuleCodec)
    <*> TomlHelper.addField
      "forSpecifiedModules"
      forSpecifiedModules
      (CompatGHC.moduleNameMapCodec forSpecifiedModuleCodec)

loadConfigFileWithFingerprint :: FilePath -> IO (Config, CompatGHC.Fingerprint)
loadConfigFileWithFingerprint filepath = do
  fingerprint <- CompatGHC.getFileHash filepath
  config <- Toml.decodeFile configCodec filepath
  pure (config, fingerprint)
