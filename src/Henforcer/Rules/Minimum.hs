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
  , minimumAllowedDecoder
  , MinimumNat
  ) where

import qualified Data.Text as T
import qualified Dhall
import qualified Numeric.Natural as Nat

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

minimumAllowedDecoder :: Dhall.Decoder MinimumAllowed
minimumAllowedDecoder =
  Dhall.union $
    fmap (const NoMinimumToEnforce) (Dhall.constructor (T.pack "NoMinimumToEnforce") Dhall.unit)
      <> fmap MinimumAllowed (Dhall.constructor (T.pack "MinimumAllowed") minimumNatDecoder)

-- | A wrapper around 'Nat.Natural' for clarity
newtype MinimumNat = MinimumNat Nat.Natural
  deriving (Num, Eq, Ord, Real, Enum, Integral)

instance CompatGHC.Outputable MinimumNat where
  ppr (MinimumNat nat) = CompatGHC.text $ show nat

minimumNatDecoder :: Dhall.Decoder MinimumNat
minimumNatDecoder =
  fmap MinimumNat Dhall.natural
