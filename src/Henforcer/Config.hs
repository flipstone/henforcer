{- |
Module      : Henforcer.Config
Description : Haskell representation of the dhall configuration file that is used to define the checks to be enforced.
Copyright   : (c) Flipstone Technology Partners, 2023
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
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC
import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Rules as Rules

data DependencyDeclaration = DependencyDeclaration
  { moduleTree :: CodeStructure.TreeName
  , treeDependencies :: [CodeStructure.TreeName]
  }

dependencyDeclarationDecoder :: Dhall.Decoder DependencyDeclaration
dependencyDeclarationDecoder =
  Dhall.record $
    DependencyDeclaration
      <$> Dhall.field (T.pack "moduleTree") CodeStructure.treeNameDecoder
      <*> Dhall.field (T.pack "dependencies") (Dhall.list CodeStructure.treeNameDecoder)

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

forAnyModuleDecoder :: Dhall.Decoder ForAnyModule
forAnyModuleDecoder =
  Dhall.record $
    ForAnyModule
      <$> Dhall.field (T.pack "treeDependencies") (Dhall.list dependencyDeclarationDecoder)
      <*> Dhall.field (T.pack "encapsulatedTrees") (Dhall.list CodeStructure.treeNameDecoder)
      <*> Dhall.field
        (T.pack "allowedQualifications")
        (Dhall.map CompatGHC.moduleNameDecoder (Dhall.list CodeStructure.qualificationSchemeDecoder))
      <*> Dhall.field
        (T.pack "allowedOpenUnaliasedImports")
        Rules.maximumAllowedDecoder
      <*> Dhall.field
        (T.pack "allowedAliasUniqueness")
        CodeStructure.allowedAliasUniquenessDecoder
      <*> Dhall.field
        (T.pack "maximumExportsPlusHeaderUndocumented")
        Rules.maximumAllowedDecoder
      <*> Dhall.field
        (T.pack "minimumExportsPlusHeaderDocumented")
        Rules.minimumAllowedDecoder
      <*> Dhall.field
        (T.pack "maximumExportsWithoutSince")
        Rules.maximumAllowedDecoder
      <*> Dhall.field
        (T.pack "minimumExportsWithSince")
        Rules.minimumAllowedDecoder
      <*> Dhall.field
        (T.pack "moduleHeaderCopyrightMustExistNonEmpty")
        Rules.mustExistNonEmptyDecoder
      <*> Dhall.field
        (T.pack "moduleHeaderDescriptionMustExistNonEmpty")
        Rules.mustExistNonEmptyDecoder
      <*> Dhall.field
        (T.pack "moduleHeaderLicenseMustExistNonEmpty")
        Rules.mustExistNonEmptyDecoder
      <*> Dhall.field
        (T.pack "moduleHeaderMaintainerMustExistNonEmpty")
        Rules.mustExistNonEmptyDecoder

data ForSpecifiedModule = ForSpecifiedModule
  { specifiedModuleAllowedOpenUnaliasedImports :: !(Maybe Rules.MaximumAllowed)
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
    { specifiedModuleAllowedOpenUnaliasedImports = Nothing
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

forSpecifiedModuleDecoder :: Dhall.Decoder ForSpecifiedModule
forSpecifiedModuleDecoder =
  Dhall.record $
    ForSpecifiedModule
      <$> Dhall.field
        (T.pack "allowedOpenUnaliasedImports")
        (Dhall.maybe Rules.maximumAllowedDecoder)
      <*> Dhall.field
        (T.pack "allowedAliasUniqueness")
        (Dhall.maybe CodeStructure.allowedAliasUniquenessDecoder)
      <*> Dhall.field
        (T.pack "maximumExportsPlusHeaderUndocumented")
        (Dhall.maybe Rules.maximumAllowedDecoder)
      <*> Dhall.field
        (T.pack "minimumExportsPlusHeaderDocumented")
        (Dhall.maybe Rules.minimumAllowedDecoder)
      <*> Dhall.field
        (T.pack "maximumExportsWithoutSince")
        (Dhall.maybe Rules.maximumAllowedDecoder)
      <*> Dhall.field
        (T.pack "minimumExportsWithSince")
        (Dhall.maybe Rules.minimumAllowedDecoder)
      <*> Dhall.field
        (T.pack "moduleHeaderCopyrightMustExistNonEmpty")
        (Dhall.maybe Rules.mustExistNonEmptyDecoder)
      <*> Dhall.field
        (T.pack "moduleHeaderDescriptionMustExistNonEmpty")
        (Dhall.maybe Rules.mustExistNonEmptyDecoder)
      <*> Dhall.field
        (T.pack "moduleHeaderLicenseMustExistNonEmpty")
        (Dhall.maybe Rules.mustExistNonEmptyDecoder)
      <*> Dhall.field
        (T.pack "moduleHeaderMaintainerMustExistNonEmpty")
        (Dhall.maybe Rules.mustExistNonEmptyDecoder)

type SpecifiedModuleMap = M.Map CompatGHC.ModuleName ForSpecifiedModule

data Config = Config
  { forAnyModule :: !ForAnyModule
  , forSpecifiedModules :: !SpecifiedModuleMap
  }

configDecoder :: Dhall.Decoder Config
configDecoder =
  Dhall.record $
    Config
      <$> Dhall.field
        (T.pack "forAnyModule")
        forAnyModuleDecoder
      <*> Dhall.field
        (T.pack "forSpecifiedModule")
        (Dhall.map CompatGHC.moduleNameDecoder forSpecifiedModuleDecoder)

loadConfigFileWithFingerprint :: FilePath -> IO (Config, CompatGHC.Fingerprint)
loadConfigFileWithFingerprint filepath = do
  fingerprint <- CompatGHC.getFileHash filepath
  config <- Dhall.inputFile configDecoder filepath
  pure (config, fingerprint)
