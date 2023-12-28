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
  , MaximumAllowed (..)
  , maximumAllowedDecoder
  , MaximumNat
  ) where

import qualified Data.Text as T
import qualified Dhall
import qualified Numeric.Natural as Nat

import qualified CompatGHC

checkMaximum ::
  MaximumAllowed
  -> a
  -> (a -> MaximumNat)
  -> (a -> MaximumNat -> b)
  -> [b]
checkMaximum maximumAllowed a getNat handleFailure =
  case maximumAllowed of
    NoMaximumToEnforce ->
      mempty
    MaximumAllowed maxNat ->
      let moduleNat = getNat a
       in if moduleNat > maxNat
            then pure $ handleFailure a maxNat
            else mempty

{- | Represent a maximum for a given module or perhaps _any_ module. This is used to collapse a
stated rule in configuration of a maximum for any module with a rule specified as applying to a
particular module into a single type and be checked against.
-}
data MaximumAllowed
  = NoMaximumToEnforce
  | MaximumAllowed !MaximumNat

maximumAllowedDecoder :: Dhall.Decoder MaximumAllowed
maximumAllowedDecoder =
  Dhall.union $
    fmap (const NoMaximumToEnforce) (Dhall.constructor (T.pack "NoMaximumToEnforce") Dhall.unit)
      <> fmap MaximumAllowed (Dhall.constructor (T.pack "MaximumAllowed") maximumNatDecoder)

-- | A wrapper around 'Nat.Natural' for clarity
newtype MaximumNat = MaximumNat Nat.Natural
  deriving (Num, Eq, Ord, Real, Enum, Integral)

instance CompatGHC.Outputable MaximumNat where
  ppr (MaximumNat nat) = CompatGHC.text $ show nat

maximumNatDecoder :: Dhall.Decoder MaximumNat
maximumNatDecoder =
  fmap MaximumNat Dhall.natural
