{- |
Module      : Henforcer.CodeStructure.Import.Scheme.Alias
Description : Models the representation of an alias portion of an import statement.
Copyright   : (c) Flipstone Technology Partners, 2023-2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.Alias
  ( Alias (..)
  , aliasCodecWithDefault
  , determineAlias
  , isAliased
  , isAliasedAs
  ) where

import qualified Toml

import qualified CompatGHC

-- | Directly represent if an import has an alias or not.
data Alias
  = WithAlias !CompatGHC.ModuleName
  | WithoutAlias
  deriving (Show, Eq, Ord)

maybeFromAlias :: Alias -> Maybe CompatGHC.ModuleName
maybeFromAlias WithoutAlias = Nothing
maybeFromAlias (WithAlias x) = Just x

aliasCodec :: Toml.Key -> Toml.TomlCodec Alias
aliasCodec =
  Toml.dimatch maybeFromAlias WithAlias . CompatGHC.moduleNameCodec

defaultWithoutAlias :: Maybe Alias -> Alias
defaultWithoutAlias Nothing = WithoutAlias
defaultWithoutAlias (Just x) = x

aliasCodecWithDefault :: Toml.Key -> Toml.TomlCodec Alias
aliasCodecWithDefault =
  Toml.dimap pure defaultWithoutAlias . Toml.dioptional . aliasCodec

-- | Compute the alias from an import
determineAlias :: CompatGHC.ImportDecl CompatGHC.GhcRn -> Alias
determineAlias =
  maybe WithoutAlias (WithAlias . CompatGHC.unLoc) . CompatGHC.ideclAs

isAliased :: Alias -> Bool
isAliased (WithAlias _) = True
isAliased WithoutAlias = False

isAliasedAs :: CompatGHC.ModuleName -> Alias -> Bool
isAliasedAs modName (WithAlias alias) = modName == alias
isAliasedAs _ WithoutAlias = False
