{- |
Module      : Henforcer.Rules.MustExistNonEmpty
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.MustExistNonEmpty
  ( MustExistNonEmpty
  , mustExistNonEmptyCodec
  , checkExistsAndNonEmptyString
  ) where

import qualified Toml

import Henforcer.Rules.ConditionallyEnforced
  ( ConditionallyEnforced (Enforced, NotEnforced)
  , conditionallyEnforcedCodec
  )

type MustExistNonEmpty = ConditionallyEnforced Bool

mustExistNonEmptyCodec :: Toml.Key -> Toml.TomlCodec MustExistNonEmpty
{-# INLINEABLE mustExistNonEmptyCodec #-}
mustExistNonEmptyCodec = conditionallyEnforcedCodec Toml.bool

checkExistsAndNonEmptyString ::
  (Applicative m, Monoid (m b)) =>
  MustExistNonEmpty
  -> a
  -> (a -> Maybe String)
  -> (a -> b)
  -> m b
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
