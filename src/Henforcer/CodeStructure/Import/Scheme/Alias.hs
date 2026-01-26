{- |
Module      : Henforcer.CodeStructure.Import.Scheme.Alias
Description : Models the representation of an alias portion of an import statement.
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.Alias
  ( Alias (..)
  , aliasCodecWithDefault
  , determineAlias
  ) where

import qualified Toml

import qualified CompatGHC

{- | Directly represent if an import has an alias or not.

@since 1.0.0.0
-}
data Alias
  = WithAlias !CompatGHC.ModuleName
  | WithoutAlias
  deriving (Eq, Show)

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

{- | Compute the alias from an import

@since 1.0.0.0
-}
determineAlias :: CompatGHC.ImportDecl CompatGHC.GhcRn -> Alias
determineAlias =
  maybe WithoutAlias (WithAlias . CompatGHC.unLoc) . CompatGHC.ideclAs
