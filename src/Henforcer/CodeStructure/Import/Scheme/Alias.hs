{- |
Module      : Henforcer.CodeStructure.Import.Scheme.Alias
Description : Models the representation of an alias portion of an import statement.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.Alias
  ( Alias (..)
  , aliasDecoder
  , determineAlias
  , isAliased
  , isAliasedAs
  ) where

import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC

-- | Directly represent if an import has an alias or not.
data Alias
  = WithAlias !CompatGHC.ModuleName
  | WithoutAlias
  deriving (Eq, Ord)

-- | Compute the alias from an import
determineAlias :: CompatGHC.ImportDecl CompatGHC.GhcRn -> Alias
determineAlias =
  maybe WithoutAlias (WithAlias . CompatGHC.unLoc) . CompatGHC.ideclAs

-- | Dhall decoder for 'Alias' so that it can be read from configuration
aliasDecoder :: Dhall.Decoder Alias
aliasDecoder =
  Dhall.union $
    (const WithoutAlias <$> Dhall.constructor (T.pack "WithoutAlias") Dhall.unit)
      <> (WithAlias <$> Dhall.constructor (T.pack "WithAlias") CompatGHC.moduleNameDecoder)

isAliased :: Alias -> Bool
isAliased (WithAlias _) = True
isAliased WithoutAlias = False

isAliasedAs :: CompatGHC.ModuleName -> Alias -> Bool
isAliasedAs modName (WithAlias alias) = modName == alias
isAliasedAs _ WithoutAlias = False
