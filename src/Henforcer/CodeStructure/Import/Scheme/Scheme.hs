{- |
Module      : Henforcer.CodeStructure.Import.Scheme.Scheme
Description : Models the style of import including qualification, alias, and safety.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.Scheme
  ( Scheme (..)
  , AllowedSchemes
  , buildScheme
  , qualificationSchemeDecoder
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC
import Henforcer.CodeStructure.Import.Scheme.Alias (Alias, aliasDecoder, determineAlias)
import Henforcer.CodeStructure.Import.Scheme.Safe (Safe, determineSafe, safeDecoder)

{- | Representation of the structure of on an import, covering the qualification, any aliasing, and
the safety.
-}
data Scheme = Scheme
  { qualification :: CompatGHC.ImportDeclQualifiedStyle
  , alias :: Alias
  , safe :: Safe
  }
  deriving (Eq)

type AllowedSchemes =
  M.Map CompatGHC.ModuleName [Scheme]

-- | Compute the 'Scheme' from an import
buildScheme :: CompatGHC.ImportDecl CompatGHC.GhcRn -> Scheme
buildScheme imp =
  Scheme
    { qualification = CompatGHC.ideclQualified imp
    , alias = determineAlias imp
    , safe = determineSafe imp
    }

-- | Dhall decoder for 'Scheme' so that it can be read from configuration
qualificationSchemeDecoder :: Dhall.Decoder Scheme
qualificationSchemeDecoder =
  Dhall.record $
    Scheme
      <$> Dhall.field (T.pack "qualification") CompatGHC.qualificationDecoder
      <*> Dhall.field (T.pack "alias") aliasDecoder
      <*> Dhall.field (T.pack "safe") safeDecoder
