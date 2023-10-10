{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      : Henforcer.CodeStructure.Import.MaxOpenUnaliasedImports
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.MaxOpenUnaliasedImports
  ( MaxOpenUnaliasedImportsNat
  , determineMaxAllowedForModule
  , MaxAllowedForModule (..)
  , maxOpenUnaliasedImportNatDecoder
  , DefaultAllowedOpenUnaliasedImports
  , defaultAllowedOpenUnaliasedImportsDecoder
  , PerModuleAllowedOpenUnaliasedImports
  , perModuleAllowedOpenUnaliasedImportsDecoder
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Dhall
import qualified Numeric.Natural as Nat

import qualified CompatGHC

data DefaultAllowedOpenUnaliasedImports
  = NoDefaultMaximum
  | DefaultMaximum !MaxOpenUnaliasedImportsNat

defaultAllowedOpenUnaliasedImportsDecoder :: Dhall.Decoder DefaultAllowedOpenUnaliasedImports
defaultAllowedOpenUnaliasedImportsDecoder =
  Dhall.union $
    fmap (const NoDefaultMaximum) (Dhall.constructor (T.pack "NoDefaultMaximum") Dhall.unit)
      <> fmap DefaultMaximum (Dhall.constructor (T.pack "DefaultMaximum") maxOpenUnaliasedImportNatDecoder)

defaultMaxToMaybe :: DefaultAllowedOpenUnaliasedImports -> Maybe MaxOpenUnaliasedImportsNat
defaultMaxToMaybe NoDefaultMaximum = Nothing
defaultMaxToMaybe (DefaultMaximum n) = Just n

newtype PerModuleAllowedOpenUnaliasedImports
  = PerModuleAllowedOpenUnaliasedImports (M.Map CompatGHC.ModuleName MaxOpenUnaliasedImportsNat)

perModuleAllowedOpenUnaliasedImportsDecoder :: Dhall.Decoder PerModuleAllowedOpenUnaliasedImports
perModuleAllowedOpenUnaliasedImportsDecoder =
  fmap
    PerModuleAllowedOpenUnaliasedImports
    (Dhall.map CompatGHC.moduleNameDecoder maxOpenUnaliasedImportNatDecoder)

lookupModule ::
  PerModuleAllowedOpenUnaliasedImports -> CompatGHC.ModuleName -> Maybe MaxOpenUnaliasedImportsNat
lookupModule (PerModuleAllowedOpenUnaliasedImports perModuleMap) =
  flip M.lookup perModuleMap

data MaxAllowedForModule
  = NoMaxToEnforce
  | MaxForModule !MaxOpenUnaliasedImportsNat

determineMaxAllowedForModule ::
  DefaultAllowedOpenUnaliasedImports
  -> PerModuleAllowedOpenUnaliasedImports
  -> CompatGHC.ModuleName
  -> MaxAllowedForModule
determineMaxAllowedForModule defaultMax perModuleMax moduleName =
  case lookupModule perModuleMax moduleName of
    Just moduleMax ->
      MaxForModule moduleMax
    Nothing ->
      maybe NoMaxToEnforce MaxForModule (defaultMaxToMaybe defaultMax)

-- | A wrapper around 'Nat.Natural' for clarity
newtype MaxOpenUnaliasedImportsNat = MaxOpenUnaliasedImportsNat Nat.Natural
  deriving (Num, Eq, Ord, Real, Enum, Integral)

instance CompatGHC.Outputable MaxOpenUnaliasedImportsNat where
  ppr (MaxOpenUnaliasedImportsNat nat) = CompatGHC.text $ show nat

maxOpenUnaliasedImportNatDecoder :: Dhall.Decoder MaxOpenUnaliasedImportsNat
maxOpenUnaliasedImportNatDecoder =
  fmap MaxOpenUnaliasedImportsNat Dhall.natural
