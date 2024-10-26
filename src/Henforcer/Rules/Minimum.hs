{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Henforcer.Rules.Minimum
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.Minimum
  ( checkMinimum
  , minimumAllowedCodec
  , MinimumAllowed
  , MinimumNat
  , minimumNatCodec
  ) where

import qualified Numeric.Natural as Nat
import qualified Toml

import qualified CompatGHC
import Henforcer.Rules.ConditionallyEnforced
  ( ConditionallyEnforced (Enforced, NotEnforced)
  , conditionallyEnforcedCodec
  )

checkMinimum ::
  MinimumAllowed
  -> a
  -> (a -> MinimumNat)
  -> (a -> MinimumNat -> b)
  -> [b]
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
minimumAllowedCodec = conditionallyEnforcedCodec minimumNatCodec

-- | A wrapper around 'Nat.Natural' for clarity
newtype MinimumNat = MinimumNat Nat.Natural
  deriving (Num, Eq, Ord, Show)

instance CompatGHC.Outputable MinimumNat where
  ppr (MinimumNat nat) = CompatGHC.text $ show nat

minimumNatCodec :: Toml.Key -> Toml.TomlCodec MinimumNat
minimumNatCodec =
  Toml.diwrap . Toml.natural
