{- |
Module      : Henforcer.CodeStructure.Import.Scheme.Safe
Description : Models the representation of the safe portion of an import statement.
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.Safe
  ( Safe
  , safeToBool
  , safeCodec
  , determineSafe
  , withoutSafe
  ) where

import qualified Toml
import qualified TomlHelper

import qualified CompatGHC

{- | Directly represent if an import is 'safe' or not.

@since 1.0.0.0
-}
newtype Safe = Safe Bool
  deriving (Eq, Show)

{- | Toml codec based on bool for 'Safe' with a default of not including 'Safe'

@since 1.0.0.0
-}
safeCodec :: Toml.Key -> Toml.TomlCodec Safe
safeCodec =
  TomlHelper.setDefault (Safe False) (Toml.diwrap . Toml.bool)

{- | Conversion to 'Bool' is used for conditional logic, notably checking failure.

@since 1.0.0.0
-}
safeToBool :: Safe -> Bool
safeToBool (Safe b) = b

{- | Compute the safety from an import

@since 1.0.0.0
-}
determineSafe :: CompatGHC.ImportDecl pass -> Safe
determineSafe =
  Safe . CompatGHC.ideclSafe

{- | Safe unset, used for tests

@since 1.0.0.0
-}
withoutSafe :: Safe
withoutSafe = Safe False
