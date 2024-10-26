{- |
Module      : Henforcer.Rules.ConditionallyEnforced
Description :
Copyright   : (c) Flipstone Technology Partners, 2024
License     : BSD-3-clause
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

conditionallyEnforcedFromMaybe :: Maybe a -> ConditionallyEnforced a
conditionallyEnforcedFromMaybe Nothing = NotEnforced
conditionallyEnforcedFromMaybe (Just x) = Enforced x

maybeFromConditionallyEnforced :: ConditionallyEnforced a -> Maybe a
maybeFromConditionallyEnforced NotEnforced = Nothing
maybeFromConditionallyEnforced (Enforced x) = Just x

conditionallyEnforcedCodec ::
  (Toml.Key -> Toml.TomlCodec a) -> Toml.Key -> Toml.TomlCodec (ConditionallyEnforced a)
conditionallyEnforcedCodec underlyingCodec =
  Toml.dimap maybeFromConditionallyEnforced conditionallyEnforcedFromMaybe
    . Toml.dioptional
    . underlyingCodec
