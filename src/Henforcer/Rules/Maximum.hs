{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Henforcer.Rules.Maximum
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.Maximum
  ( checkMaximum
  , MaximumAllowed
  , maximumAllowedCodec
  , MaximumNat
  , maximumNatCodec
  ) where

import qualified Numeric.Natural as Nat
import qualified Toml

import qualified CompatGHC
import Henforcer.Rules.ConditionallyEnforced
  ( ConditionallyEnforced (Enforced, NotEnforced)
  , conditionallyEnforcedCodec
  )

checkMaximum ::
  MaximumAllowed
  -> a
  -> (a -> MaximumNat)
  -> (a -> MaximumNat -> b)
  -> [b]
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
maximumAllowedCodec = conditionallyEnforcedCodec maximumNatCodec

-- | A wrapper around 'Nat.Natural' for clarity
newtype MaximumNat = MaximumNat Nat.Natural
  deriving (Num, Eq, Ord)

instance CompatGHC.Outputable MaximumNat where
  ppr (MaximumNat nat) = CompatGHC.text $ show nat

maximumNatCodec :: Toml.Key -> Toml.TomlCodec MaximumNat
maximumNatCodec =
  Toml.diwrap . Toml.natural
