{- |
Module      : Henforcer.Rules.ConditionallyEnforced
Description :
Copyright   : (c) Flipstone Technology Partners, 2024-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.ConditionallyEnforced
  ( ConditionallyEnforced (..)
  , conditionallyEnforcedCodec
  ) where

import qualified Toml

-- | A more expressive 'Maybe'
data ConditionallyEnforced a
  = Enforced !a
  | NotEnforced
  deriving (Eq, Show)

conditionallyEnforcedCodec ::
  (Toml.Key -> Toml.TomlCodec a) -> Toml.Key -> Toml.TomlCodec (ConditionallyEnforced a)
{-# INLINEABLE conditionallyEnforcedCodec #-}
conditionallyEnforcedCodec underlyingCodec =
  Toml.dimap maybeFromConditionallyEnforced conditionallyEnforcedFromMaybe
    . Toml.dioptional
    . underlyingCodec

conditionallyEnforcedFromMaybe :: Maybe a -> ConditionallyEnforced a
conditionallyEnforcedFromMaybe Nothing = NotEnforced
conditionallyEnforcedFromMaybe (Just x) = Enforced x

maybeFromConditionallyEnforced :: ConditionallyEnforced a -> Maybe a
maybeFromConditionallyEnforced NotEnforced = Nothing
maybeFromConditionallyEnforced (Enforced x) = Just x
