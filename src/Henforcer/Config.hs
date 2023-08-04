{- |
Module      : Henforcer.Config
Description : Haskell representation of the dhall configuration file that is used to define the checks to be enforced.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config
  ( Config (..)
  , DependencyDeclaration (..)
  , loadConfigFileWithFingerprint
  ) where

import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC
import qualified Henforcer.CodeStructure as CodeStructure

data Config = Config
  { dependencyDeclarations :: [DependencyDeclaration]
  , encapsulatedTrees :: [CodeStructure.TreeName]
  , allowedQualifications :: CodeStructure.AllowedSchemes
  , defaultAllowedOpenUnaliasedImports :: CodeStructure.DefaultAllowedOpenUnaliasedImports
  , perModuleOpenUnaliasedImports :: CodeStructure.PerModuleAllowedOpenUnaliasedImports
  }

data DependencyDeclaration = DependencyDeclaration
  { moduleTree :: CodeStructure.TreeName
  , treeDependencies :: [CodeStructure.TreeName]
  }

loadConfigFileWithFingerprint :: FilePath -> IO (Config, CompatGHC.Fingerprint)
loadConfigFileWithFingerprint filepath = do
  fingerprint <- CompatGHC.getFileHash filepath
  config <- Dhall.inputFile configDecoder filepath
  pure (config, fingerprint)

configDecoder :: Dhall.Decoder Config
configDecoder =
  Dhall.record $
    Config
      <$> Dhall.field (T.pack "treeDependencies") (Dhall.list dependencyDeclarationDecoder)
      <*> Dhall.field (T.pack "encapsulatedTrees") (Dhall.list CodeStructure.treeNameDecoder)
      <*> Dhall.field
        (T.pack "allowedQualifications")
        (Dhall.map CompatGHC.moduleNameDecoder (Dhall.list CodeStructure.qualificationSchemeDecoder))
      <*> Dhall.field
        (T.pack "defaultAllowedOpenUnaliasedImports")
        CodeStructure.defaultAllowedOpenUnaliasedImportsDecoder
      <*> Dhall.field
        (T.pack "perModuleOpenUnaliasedImports")
        CodeStructure.perModuleAllowedOpenUnaliasedImportsDecoder

dependencyDeclarationDecoder :: Dhall.Decoder DependencyDeclaration
dependencyDeclarationDecoder =
  Dhall.record $
    DependencyDeclaration
      <$> Dhall.field (T.pack "moduleTree") CodeStructure.treeNameDecoder
      <*> Dhall.field (T.pack "dependencies") (Dhall.list CodeStructure.treeNameDecoder)
