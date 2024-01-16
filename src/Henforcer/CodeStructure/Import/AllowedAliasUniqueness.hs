{- |
Module      : Henforcer.CodeStructure.Import.AllowedAliasUniqueness
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.AllowedAliasUniqueness
  ( AllowedAliasUniqueness (..)
  , allowedAliasUniquenessCodec
  ) where

import qualified Data.Set as Set
import qualified Toml

import qualified CompatGHC
import qualified TomlHelper

data AllowedAliasUniqueness
  = AllAliasesUniqueExcept !(Set.Set CompatGHC.ModuleName)
  | AliasesToBeUnique !(Set.Set CompatGHC.ModuleName)
  | NoAliasUniqueness
  deriving (Show)

data Intermediate = Intermediate
  { allAliasesUnique :: !Bool
  , aliases :: !(Set.Set CompatGHC.ModuleName)
  }

intermediateCodec :: Toml.TomlCodec Intermediate
intermediateCodec =
  Intermediate
    <$> TomlHelper.addField "allAliasesUnique" allAliasesUnique Toml.bool
    <*> TomlHelper.addField "aliases" aliases (Toml.dimap Set.toList Set.fromList . CompatGHC.moduleNameListCodec)

intermediateTo :: Maybe Intermediate -> AllowedAliasUniqueness
intermediateTo Nothing = NoAliasUniqueness
intermediateTo (Just i) =
  if allAliasesUnique i
    then AllAliasesUniqueExcept (aliases i)
    else AliasesToBeUnique (aliases i)

toIntermediate :: AllowedAliasUniqueness -> Maybe Intermediate
toIntermediate NoAliasUniqueness = Nothing
toIntermediate (AllAliasesUniqueExcept a) = Just (Intermediate True a)
toIntermediate (AliasesToBeUnique a) = Just (Intermediate False a)

allowedAliasUniquenessCodec :: Toml.Key -> Toml.TomlCodec AllowedAliasUniqueness
allowedAliasUniquenessCodec =
  Toml.dimap toIntermediate intermediateTo . Toml.dioptional . Toml.table intermediateCodec
