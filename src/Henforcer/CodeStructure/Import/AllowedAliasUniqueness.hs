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
import qualified Henforcer.Rules.UserNote as UserNote
import qualified TomlHelper

data AllowedAliasUniqueness
  = AllAliasesUniqueExcept !(Set.Set CompatGHC.ModuleName) UserNote.UserNote
  | AliasesToBeUnique !(Set.Set CompatGHC.ModuleName) UserNote.UserNote
  | NoAliasUniqueness
  deriving (Show)

data Intermediate = Intermediate
  { allAliasesUnique :: !Bool
  , aliases :: !(Set.Set CompatGHC.ModuleName)
  , aliasUniquenessNote :: UserNote.UserNote
  }

intermediateCodec :: Toml.TomlCodec Intermediate
intermediateCodec =
  Intermediate
    <$> TomlHelper.addField "allAliasesUnique" allAliasesUnique Toml.bool
    <*> TomlHelper.addField
      "aliases"
      aliases
      (Toml.dimap Set.toList Set.fromList . CompatGHC.moduleNameListCodec)
    <*> UserNote.userNoteField aliasUniquenessNote

intermediateTo :: Maybe Intermediate -> AllowedAliasUniqueness
intermediateTo Nothing = NoAliasUniqueness
intermediateTo (Just i) =
  if allAliasesUnique i
    then AllAliasesUniqueExcept (aliases i) (aliasUniquenessNote i)
    else AliasesToBeUnique (aliases i) (aliasUniquenessNote i)

toIntermediate :: AllowedAliasUniqueness -> Maybe Intermediate
toIntermediate NoAliasUniqueness = Nothing
toIntermediate (AllAliasesUniqueExcept a n) = Just (Intermediate True a n)
toIntermediate (AliasesToBeUnique a n) = Just (Intermediate False a n)

allowedAliasUniquenessCodec :: Toml.Key -> Toml.TomlCodec AllowedAliasUniqueness
allowedAliasUniquenessCodec =
  Toml.dimap toIntermediate intermediateTo . Toml.dioptional . Toml.table intermediateCodec
