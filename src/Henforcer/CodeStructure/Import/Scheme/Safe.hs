{- |
Module      : Henforcer.CodeStructure.Import.Scheme.Safe
Description : Models the representation of the safe portion of an import statement.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.Safe
  ( Safe (..)
  , safeDecoder
  , determineSafe
  ) where

import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC

-- | Directly represent if an import is 'safe' or not.
data Safe
  = WithSafe
  | WithoutSafe
  deriving (Eq)

-- | Compute the safety from an import
determineSafe :: CompatGHC.ImportDecl pass -> Safe
determineSafe imp =
  if CompatGHC.ideclSafe imp
    then WithSafe
    else WithoutSafe

-- | Dhall decoder for 'Safe' so that it can be read from configuration
safeDecoder :: Dhall.Decoder Safe
safeDecoder =
  Dhall.union $
    (const WithoutSafe <$> Dhall.constructor (T.pack "WithoutSafe") Dhall.unit)
      <> (const WithSafe <$> Dhall.constructor (T.pack "WithSafe") Dhall.unit)
