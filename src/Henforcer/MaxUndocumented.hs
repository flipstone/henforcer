{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Henforcer.CodeStructure.MaxUndocumentedExports
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.MaxUndocumented
  ( MaxUndocumentedExportsNat
  , determineMaxAllowedForModule
  , MaxAllowedForModule (..)
  , maxUndocumentedImportNatDecoder
  , DefaultAllowedUndocumentedExports
  , defaultAllowedUndocumentedExportsDecoder
  , PerModuleAllowedUndocumentedExports
  , perModuleAllowedUndocumentedExportsDecoder
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Dhall
import qualified Numeric.Natural as Nat

import qualified CompatGHC

data DefaultAllowedUndocumentedExports
  = NoDefaultMaximum
  | DefaultMaximum !MaxUndocumentedExportsNat

defaultAllowedUndocumentedExportsDecoder :: Dhall.Decoder DefaultAllowedUndocumentedExports
defaultAllowedUndocumentedExportsDecoder =
  Dhall.union $
    fmap (const NoDefaultMaximum) (Dhall.constructor (T.pack "NoDefaultMaximumUndocumented") Dhall.unit)
      <> fmap DefaultMaximum (Dhall.constructor (T.pack "DefaultMaximumUndocumented") maxUndocumentedImportNatDecoder)

defaultMaxToMaybe :: DefaultAllowedUndocumentedExports -> Maybe MaxUndocumentedExportsNat
defaultMaxToMaybe NoDefaultMaximum = Nothing
defaultMaxToMaybe (DefaultMaximum n) = Just n

newtype PerModuleAllowedUndocumentedExports
  = PerModuleAllowedUndocumentedExports (M.Map CompatGHC.ModuleName MaxUndocumentedExportsNat)

perModuleAllowedUndocumentedExportsDecoder :: Dhall.Decoder PerModuleAllowedUndocumentedExports
perModuleAllowedUndocumentedExportsDecoder =
  fmap
    PerModuleAllowedUndocumentedExports
    (Dhall.map CompatGHC.moduleNameDecoder maxUndocumentedImportNatDecoder)

lookupModule ::
  PerModuleAllowedUndocumentedExports -> CompatGHC.ModuleName -> Maybe MaxUndocumentedExportsNat
lookupModule (PerModuleAllowedUndocumentedExports perModuleMap) =
  flip M.lookup perModuleMap

data MaxAllowedForModule
  = NoMaxToEnforce
  | MaxForModule !MaxUndocumentedExportsNat

determineMaxAllowedForModule ::
  DefaultAllowedUndocumentedExports
  -> PerModuleAllowedUndocumentedExports
  -> CompatGHC.ModuleName
  -> MaxAllowedForModule
determineMaxAllowedForModule defaultMax perModuleMax moduleName =
  case lookupModule perModuleMax moduleName of
    Just moduleMax ->
      MaxForModule moduleMax
    Nothing ->
      maybe NoMaxToEnforce MaxForModule (defaultMaxToMaybe defaultMax)

-- | A wrapper around 'Nat.Natural' for clarity
newtype MaxUndocumentedExportsNat = MaxUndocumentedExportsNat Nat.Natural
  deriving (Num, Eq, Ord, Real, Enum, Integral)

instance CompatGHC.Outputable MaxUndocumentedExportsNat where
  ppr (MaxUndocumentedExportsNat nat) = CompatGHC.text $ show nat

maxUndocumentedImportNatDecoder :: Dhall.Decoder MaxUndocumentedExportsNat
maxUndocumentedImportNatDecoder =
  fmap MaxUndocumentedExportsNat Dhall.natural
