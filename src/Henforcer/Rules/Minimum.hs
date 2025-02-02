{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Henforcer.Rules.Minimum
Description : Generic rule for allowing enforcement of a lower bound
Copyright   : (c) Flipstone Technology Partners, 2023-2025
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.Minimum
  ( checkMinimum
  , MinimumAllowed
  , minimumAllowedCodec
  , MinimumNat
  ) where

import qualified Numeric.Natural as Nat
import qualified Toml

import qualified CompatGHC
import Henforcer.Rules.ConditionallyEnforced
  ( ConditionallyEnforced (Enforced, NotEnforced)
  , conditionallyEnforcedCodec
  )

{- | If a 'MinimumAllowed' is to be enforced, then for a given value, get

@since 1.0.0.0
-}
checkMinimum ::
  (Applicative m, Monoid (m b)) =>
  MinimumAllowed
  -> a
  -> (a -> MinimumNat)
  -> (a -> MinimumNat -> b)
  -> m b
checkMinimum minimumAllowed a getNat handleFailure =
  case minimumAllowed of
    NotEnforced ->
      mempty
    Enforced minNat ->
      let moduleNat = getNat a
       in if moduleNat < minNat
            then pure $ handleFailure a minNat
            else mempty

type MinimumAllowed = ConditionallyEnforced MinimumNat

minimumAllowedCodec :: Toml.Key -> Toml.TomlCodec MinimumAllowed
{-# INLINEABLE minimumAllowedCodec #-}
minimumAllowedCodec = conditionallyEnforcedCodec minimumNatCodec

{- | A wrapper around 'Nat.Natural' for clarity

@since 1.0.0.0
-}
newtype MinimumNat = MinimumNat Nat.Natural
  deriving (Num, Eq, Ord, Show)

{- |

@since 1.0.0.0
-}
instance CompatGHC.Outputable MinimumNat where
  ppr (MinimumNat nat) = CompatGHC.text $ show nat

minimumNatCodec :: Toml.Key -> Toml.TomlCodec MinimumNat
minimumNatCodec =
  Toml.diwrap . Toml.natural
