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
  , keepOnlyPackageNameInQualifier
  ) where

import qualified Control.Monad as M
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC
import Henforcer.CodeStructure.Import.Scheme.Alias (Alias, aliasDecoder, determineAlias)
import Henforcer.CodeStructure.Import.Scheme.PackageQualifier
  ( PackageQualifier (WithPackageQualifier, WithoutPackageQualifier)
  , determinePackageQualifier
  , keepPackageNameOnly
  , packageQualifierDecoder
  )
import Henforcer.CodeStructure.Import.Scheme.Safe (Safe, determineSafe, safeDecoder)

{- | Representation of the structure of on an import, covering the qualification, any aliasing, and
the safety.
-}
data Scheme = Scheme
  { qualification :: !CompatGHC.ImportDeclQualifiedStyle
  , alias :: !Alias
  , safe :: !Safe
  , packageQualification :: !PackageQualifier
  }
  deriving (Eq)

type AllowedSchemes =
  Map.Map CompatGHC.ModuleName [Scheme]

-- | Compute the 'Scheme' from an import
buildScheme :: CompatGHC.ImportDecl CompatGHC.GhcRn -> Scheme
buildScheme imp =
  Scheme
    { qualification = CompatGHC.ideclQualified imp
    , alias = determineAlias imp
    , safe = determineSafe imp
    , packageQualification = determinePackageQualifier imp
    }

-- | Dhall decoder for 'Scheme' so that it can be read from configuration
qualificationSchemeDecoder :: Dhall.Decoder Scheme
qualificationSchemeDecoder =
  Dhall.record $
    Scheme
      <$> Dhall.field (T.pack "qualification") CompatGHC.qualificationDecoder
      <*> Dhall.field (T.pack "alias") aliasDecoder
      <*> Dhall.field (T.pack "safe") safeDecoder
      <*> Dhall.field (T.pack "packageQualification") packageQualifierDecoder

keepOnlyPackageNameInQualifier :: Scheme -> Scheme
keepOnlyPackageNameInQualifier s =
  let
    mbGetQualifierText :: PackageQualifier -> Maybe T.Text
    mbGetQualifierText (WithPackageQualifier t) = Just t
    mbGetQualifierText WithoutPackageQualifier = Nothing

    newQualifier =
      case M.join . fmap keepPackageNameOnly . mbGetQualifierText $ packageQualification s of
        Nothing -> WithoutPackageQualifier
        Just t -> WithPackageQualifier t
   in
    s{packageQualification = newQualifier}
