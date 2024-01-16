{- |
Module      : Henforcer.CodeStructure.Import.Scheme.Safe
Description : Models the representation of the safe portion of an import statement.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.Safe
  ( Safe (..)
  , safeCodec
  , determineSafe
  ) where

import qualified Toml

import qualified CompatGHC

-- TODO Safe can be a newtype wrapper around bool, to save some conversions

-- | Directly represent if an import is 'safe' or not.
data Safe
  = WithSafe
  | WithoutSafe
  deriving (Show, Eq)

-- | Toml codec based on bool for 'Safe'
safeCodec :: Toml.Key -> Toml.TomlCodec Safe
safeCodec =
  Toml.dimap safeToBool boolToSafe . Toml.bool

safeToBool :: Safe -> Bool
safeToBool WithSafe = True
safeToBool WithoutSafe = False

boolToSafe :: Bool -> Safe
boolToSafe True = WithSafe
boolToSafe False = WithoutSafe

-- | Compute the safety from an import
determineSafe :: CompatGHC.ImportDecl pass -> Safe
determineSafe imp =
  if CompatGHC.ideclSafe imp
    then WithSafe
    else WithoutSafe
