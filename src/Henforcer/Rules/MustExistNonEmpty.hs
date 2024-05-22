{- |
Module      : Henforcer.Rules.MustExistNonEmpty
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.MustExistNonEmpty
  ( MustExistNonEmpty
  , mustExistNonEmptyCodec
  , checkExistsAndNonEmptyString
  ) where

import qualified Toml

import Henforcer.Rules.ConditionallyEnforced (ConditionallyEnforced(Enforced, NotEnforced), conditionallyEnforcedCodec)

type MustExistNonEmpty = ConditionallyEnforced Bool

mustExistNonEmptyCodec :: Toml.Key -> Toml.TomlCodec MustExistNonEmpty
mustExistNonEmptyCodec = conditionallyEnforcedCodec Toml.bool

checkExistsAndNonEmptyString ::
  MustExistNonEmpty
  -> a
  -> (a -> Maybe String)
  -> (a -> b)
  -> [b]
checkExistsAndNonEmptyString mustExistNonEmpty a getStr handleFailure =
  case mustExistNonEmpty of
    NotEnforced ->
      mempty
    Enforced False ->
      mempty
    Enforced True ->
      case getStr a of
        Nothing ->
          pure $ handleFailure a
        Just "" ->
          pure $ handleFailure a
        Just _ ->
          mempty
