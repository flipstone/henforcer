{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Henforcer.CodeStructure.Import.Scheme.Scheme
Description : Models the style of import including qualification, alias, and safety.
Copyright   : (c) Flipstone Technology Partners, 2023-2025
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.Scheme
  ( Scheme (..)
  , SchemeWithNote (..)
  , AllowedSchemes
  , buildScheme
  , allowedSchemesCodec
  , keepOnlyPackageNameInQualifier
  ) where

import qualified Control.Monad as M
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Toml

import qualified CompatGHC
import Henforcer.CodeStructure.Import.Scheme.Alias
  ( Alias
  , aliasCodecWithDefault
  , determineAlias
  )
import Henforcer.CodeStructure.Import.Scheme.PackageQualifier
  ( PackageQualifier (WithPackageQualifier, WithoutPackageQualifier)
  , determinePackageQualifier
  , keepPackageNameOnly
  , packageQualifierCodec
  )
import Henforcer.CodeStructure.Import.Scheme.Safe (Safe, determineSafe, safeCodec)
import qualified Henforcer.Rules as Rules
import qualified TomlHelper

{- | Representation of the structure of on an import, covering the qualification, any aliasing, and
the safety.

@since 1.0.0.0
-}
data Scheme = Scheme
  { qualification :: !CompatGHC.ImportDeclQualifiedStyle
  , alias :: !Alias
  , safe :: !Safe
  , packageQualification :: !PackageQualifier
  }
  deriving (Eq, Show)

{- | The 'Scheme' along with the 'UserNote' that was requested to be printed in the case of failures.

@since 1.0.0.0
-}
data SchemeWithNote = SchemeWithNote
  { schemeNote :: !Rules.UserNote
  , underlyingScheme :: !Scheme
  }
  deriving (Eq, Show)

{- | The 'SchemeWithNote's that should be allowed for each 'CompatGHC.ModuleName'.

@since 1.0.0.0
-}
type AllowedSchemes =
  Map.Map CompatGHC.ModuleName [SchemeWithNote]

{- | Conversion for 'AllowedSchemes' that performs conversion through 'SchemeToml'.

@since 1.0.0.0
-}
allowedSchemesCodec :: Toml.Key -> Toml.TomlCodec AllowedSchemes
allowedSchemesCodec =
  Toml.dimap ((fmap . fmap) schemeToToml) (fmap tomlsToSchemes)
    . Toml.map (CompatGHC.moduleNameCodec "module") (Toml.list schemeTomlCodec "importScheme")

{- | Compute the 'Scheme' from an import

@since 1.0.0.0
-}
buildScheme :: CompatGHC.ImportDecl CompatGHC.GhcRn -> Scheme
buildScheme imp =
  Scheme
    { qualification = CompatGHC.ideclQualified imp
    , alias = determineAlias imp
    , safe = determineSafe imp
    , packageQualification = determinePackageQualifier imp
    }

{- | The Toml verion of an import scheme. This is an intermediate type to facilitate configuration in
   a more ergonomic manner.

@since 1.0.0.0
-}
data SchemeToml = SchemeToml
  { tomlAlias :: !Alias
  , tomlSafe :: !Safe
  , tomlPackageQualification :: !PackageQualifier
  , tomlQualification :: !QualificationToml
  , tomlSchemeNote :: !Rules.UserNote
  }

schemeTomlCodec :: Toml.TomlCodec SchemeToml
schemeTomlCodec =
  SchemeToml
    <$> TomlHelper.addField "alias" tomlAlias aliasCodecWithDefault
    <*> TomlHelper.addField "safe" tomlSafe safeCodec
    <*> TomlHelper.addField
      "packageQualified"
      tomlPackageQualification
      (TomlHelper.setDefault WithoutPackageQualifier packageQualifierCodec)
    <*> TomlHelper.addField "qualified" tomlQualification (Toml.table qualificationTomlCodec)
    <*> Rules.userNoteField tomlSchemeNote

schemeToToml :: SchemeWithNote -> SchemeToml
schemeToToml withNote =
  let
    s = underlyingScheme withNote
   in
    SchemeToml
      { tomlAlias = alias s
      , tomlSafe = safe s
      , tomlPackageQualification = packageQualification s
      , tomlQualification = qualificationStyleToToml $ qualification s
      , tomlSchemeNote = schemeNote withNote
      }

tomlsToSchemes :: [SchemeToml] -> [SchemeWithNote]
tomlsToSchemes =
  M.join . fmap explodeSchemeToml

explodeSchemeToml :: SchemeToml -> [SchemeWithNote]
explodeSchemeToml st =
  explodeQualificationToml
    (tomlSchemeNote st)
    (tomlAlias st)
    (tomlSafe st)
    (tomlPackageQualification st)
    (tomlQualification st)

{- | Filters out version and seperators so we can look only at the package portion for comparsion.

@since 1.0.0.0
-}
keepOnlyPackageNameInQualifier :: Scheme -> Scheme
keepOnlyPackageNameInQualifier s =
  let
    mbGetQualifierText :: PackageQualifier -> Maybe T.Text
    mbGetQualifierText (WithPackageQualifier t) = Just t
    mbGetQualifierText WithoutPackageQualifier = Nothing

    newQualifier =
      maybe WithoutPackageQualifier WithPackageQualifier
        . M.join
        . fmap keepPackageNameOnly
        . mbGetQualifierText
        $ packageQualification s
   in
    s{packageQualification = newQualifier}

{- | Representation of the choices for qualification as it is presented in the toml config.

@since 1.0.0.0
-}
data QualificationToml = QualificationToml
  { qualifiedPre :: !Bool
  , qualifiedPost :: !Bool
  , unqualified :: !Bool
  }

qualificationTomlCodec :: Toml.TomlCodec QualificationToml
qualificationTomlCodec =
  QualificationToml
    <$> TomlHelper.addField "qualifiedPre" qualifiedPre (TomlHelper.setDefault False Toml.bool)
    <*> TomlHelper.addField "qualifiedPost" qualifiedPost (TomlHelper.setDefault False Toml.bool)
    <*> TomlHelper.addField "unqualified" unqualified (TomlHelper.setDefault False Toml.bool)

qualificationTomlToQualifications :: QualificationToml -> [CompatGHC.ImportDeclQualifiedStyle]
qualificationTomlToQualifications i =
  case i of
    (QualificationToml False False False) -> []
    (QualificationToml True False False) -> [CompatGHC.QualifiedPre]
    (QualificationToml False True False) -> [CompatGHC.QualifiedPost]
    (QualificationToml False False True) -> [CompatGHC.NotQualified]
    (QualificationToml True True False) -> [CompatGHC.QualifiedPre, CompatGHC.QualifiedPost]
    (QualificationToml True False True) -> [CompatGHC.QualifiedPre, CompatGHC.NotQualified]
    (QualificationToml False True True) -> [CompatGHC.QualifiedPost, CompatGHC.NotQualified]
    (QualificationToml True True True) -> [CompatGHC.QualifiedPre, CompatGHC.QualifiedPost, CompatGHC.NotQualified]

qualificationStyleToToml :: CompatGHC.ImportDeclQualifiedStyle -> QualificationToml
qualificationStyleToToml CompatGHC.QualifiedPre = QualificationToml True False False
qualificationStyleToToml CompatGHC.QualifiedPost = QualificationToml False True False
qualificationStyleToToml CompatGHC.NotQualified = QualificationToml False False True

explodeQualificationToml ::
  Rules.UserNote -> Alias -> Safe -> PackageQualifier -> QualificationToml -> [SchemeWithNote]
explodeQualificationToml note a s p =
  let
    mkScheme q =
      SchemeWithNote note (Scheme q a s p)
   in
    fmap mkScheme . qualificationTomlToQualifications
