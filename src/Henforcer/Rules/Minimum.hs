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
  , MinimumAllowed
  , minimumAllowedCodec
  , MinimumNat
  , minimumNatCodec
  ) where

import qualified Numeric.Natural as Nat
import qualified Toml

import qualified CompatGHC

checkMinimum ::
  MinimumAllowed
  -> a
  -> (a -> MinimumNat)
  -> (a -> MinimumNat -> b)
  -> [b]
checkMinimum minimumAllowed a getNat handleFailure =
  case minimumAllowed of
    NoMinimumToEnforce ->
      mempty
    MinimumAllowed minNat ->
      let moduleNat = getNat a
       in if moduleNat < minNat
            then pure $ handleFailure a minNat
            else mempty

{- | Represent a minimum for a given module or perhaps _any_ module. This is used to collapse a
stated rule in configuration of a minimum for any module with a rule specified as applying to a
particular module into a single type and be checked against.
-}
data MinimumAllowed
  = NoMinimumToEnforce
  | MinimumAllowed !MinimumNat
  deriving (Show)

minimumAllowedFromMaybe :: Maybe MinimumNat -> MinimumAllowed
minimumAllowedFromMaybe Nothing = NoMinimumToEnforce
minimumAllowedFromMaybe (Just x) = MinimumAllowed x

maybeFromMinimumAllowed :: MinimumAllowed -> Maybe MinimumNat
maybeFromMinimumAllowed NoMinimumToEnforce = Nothing
maybeFromMinimumAllowed (MinimumAllowed x) = Just x

minimumAllowedCodec :: Toml.Key -> Toml.TomlCodec MinimumAllowed
minimumAllowedCodec =
  Toml.dimap maybeFromMinimumAllowed minimumAllowedFromMaybe . Toml.dioptional . minimumNatCodec

-- | A wrapper around 'Nat.Natural' for clarity
newtype MinimumNat = MinimumNat Nat.Natural
  deriving (Num, Eq, Ord, Real, Enum, Integral, Show)

instance CompatGHC.Outputable MinimumNat where
  ppr (MinimumNat nat) = CompatGHC.text $ show nat

minimumNatCodec :: Toml.Key -> Toml.TomlCodec MinimumNat
minimumNatCodec =
  Toml.diwrap . Toml.natural
