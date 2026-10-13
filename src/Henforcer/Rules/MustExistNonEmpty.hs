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
  , mustExistNonEmptyIsEnforced
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

{- | Unlike other rules a 'MustExistNonEmpty' may be enforced but configured not to require
anything, which performs no check at all.
-}
mustExistNonEmptyIsEnforced :: MustExistNonEmpty -> Bool
mustExistNonEmptyIsEnforced NotEnforced = False
mustExistNonEmptyIsEnforced (Enforced mustExist) = mustExist

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
