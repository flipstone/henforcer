{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Henforcer.Rules.Maximum
Description : Generic rule for allowing enforcement of an upper bound
Copyright   : (c) Flipstone Technology Partners, 2023-2025
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.Maximum
  ( checkMaximum
  , MaximumAllowed
  , maximumAllowedCodec
  , MaximumNat
  ) where

import qualified Numeric.Natural as Nat
import qualified Toml

import qualified CompatGHC
import Henforcer.Rules.ConditionallyEnforced
  ( ConditionallyEnforced (Enforced, NotEnforced)
  , conditionallyEnforcedCodec
  )

checkMaximum ::
  (Applicative m, Monoid (m b)) =>
  MaximumAllowed
  -> a
  -> (a -> MaximumNat)
  -> (a -> MaximumNat -> b)
  -> m b
checkMaximum maximumAllowed a getNat handleFailure =
  case maximumAllowed of
    NotEnforced ->
      mempty
    Enforced maxNat ->
      let moduleNat = getNat a
       in if moduleNat > maxNat
            then pure $ handleFailure a maxNat
            else mempty

type MaximumAllowed = ConditionallyEnforced MaximumNat

maximumAllowedCodec :: Toml.Key -> Toml.TomlCodec MaximumAllowed
{-# INLINEABLE maximumAllowedCodec #-}
maximumAllowedCodec = conditionallyEnforcedCodec maximumNatCodec

-- | A wrapper around 'Nat.Natural' for clarity
newtype MaximumNat = MaximumNat Nat.Natural
  deriving (Num, Eq, Ord, Show)

instance CompatGHC.Outputable MaximumNat where
  ppr = CompatGHC.text . show . maximumNatToNatural

maximumNatToNatural :: MaximumNat -> Nat.Natural
maximumNatToNatural (MaximumNat nat) = nat

maximumNatCodec :: Toml.Key -> Toml.TomlCodec MaximumNat
maximumNatCodec =
  Toml.diwrap . Toml.natural
