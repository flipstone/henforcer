{- |
Module      : Henforcer.CodeStructure.Import.AllowedAliasUniqueness
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.AllowedAliasUniqueness
  ( AllowedAliasUniqueness (..)
  , allowedAliasUniquenessCodec
  , AliasUniquenessExceptions
    ( AliasUniquenessExceptions
    , aliasUniquenessExceptionsAliases
    , aliasUniquenessExceptionsNote
    )
  , AliasUniquenessRequired (AliasUniquenessRequired)
  ) where

import Control.Applicative ((<|>))
import qualified Data.Set as Set
import qualified Toml

import qualified CompatGHC
import qualified Henforcer.Rules.UserNote as UserNote
import qualified TomlHelper

data AllowedAliasUniqueness
  = AllAliasesUniqueExcept !AliasUniquenessExceptions
  | AliasesToBeUnique !AliasUniquenessRequired
  deriving (Eq, Show)

data AliasUniquenessExceptions = AliasUniquenessExceptions
  { aliasUniquenessExceptionsAliases :: !(Set.Set CompatGHC.ModuleName)
  , aliasUniquenessExceptionsNote :: !UserNote.UserNote
  }
  deriving (Eq, Show)

data AliasUniquenessRequired = AliasUniquenessRequired
  { aliasUniquenessRequiredAliases :: !(Set.Set CompatGHC.ModuleName)
  , aliasUniquenessRequiredNote :: !UserNote.UserNote
  }
  deriving (Eq, Show)

matchExcept :: AllowedAliasUniqueness -> Maybe AliasUniquenessExceptions
matchExcept (AllAliasesUniqueExcept excepts) = Just excepts
matchExcept (AliasesToBeUnique _) = Nothing

matchUnique :: AllowedAliasUniqueness -> Maybe AliasUniquenessRequired
matchUnique (AllAliasesUniqueExcept _) = Nothing
matchUnique (AliasesToBeUnique uniqs) = Just uniqs

aliasUniquenessExceptionsCodec :: Toml.TomlCodec AliasUniquenessExceptions
aliasUniquenessExceptionsCodec =
  AliasUniquenessExceptions
    <$> TomlHelper.addField
      "allAliasesUniqueExcept"
      aliasUniquenessExceptionsAliases
      (Toml.dimap Set.toList Set.fromList . CompatGHC.moduleNameListCodec)
    <*> UserNote.userNoteField aliasUniquenessExceptionsNote

aliasUniquenessRequiredCodec :: Toml.TomlCodec AliasUniquenessRequired
aliasUniquenessRequiredCodec =
  AliasUniquenessRequired
    <$> TomlHelper.addField
      "uniqueAliases"
      aliasUniquenessRequiredAliases
      (Toml.dimap Set.toList Set.fromList . CompatGHC.moduleNameListCodec)
    <*> UserNote.userNoteField aliasUniquenessRequiredNote

allowedAliasUniquenessCodec :: Toml.TomlCodec AllowedAliasUniqueness
allowedAliasUniquenessCodec =
  Toml.dimatch matchExcept AllAliasesUniqueExcept aliasUniquenessExceptionsCodec
    <|> Toml.dimatch matchUnique AliasesToBeUnique aliasUniquenessRequiredCodec
