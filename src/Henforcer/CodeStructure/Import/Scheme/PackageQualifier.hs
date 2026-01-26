{- |
Module      : Henforcer.CodeStructure.Import.Scheme.PackageQualified
Description : Models the representation of a package qualifier portion of an import statement.
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.PackageQualifier
  ( PackageQualifier (..)
  , packageQualifierCodec
  , determinePackageQualifier
  , keepPackageNameOnly
  ) where

import qualified Data.Text as T
import qualified Toml

import qualified CompatGHC

{- | Directly represent if an import has a package qualifier or not.

@since 1.0.0.0
-}
data PackageQualifier
  = WithPackageQualifier !T.Text
  | WithoutPackageQualifier
  deriving (Eq, Show)

maybeFromPackageQualifier :: PackageQualifier -> Maybe T.Text
maybeFromPackageQualifier WithoutPackageQualifier = Nothing
maybeFromPackageQualifier (WithPackageQualifier x) = Just x

packageQualifierCodec :: Toml.Key -> Toml.TomlCodec PackageQualifier
{-# INLINEABLE packageQualifierCodec #-}
packageQualifierCodec =
  Toml.dimatch maybeFromPackageQualifier WithPackageQualifier . Toml.text

{- | Compute the packageQualifier from an import

@since 1.0.0.0
-}
determinePackageQualifier :: CompatGHC.ImportDecl CompatGHC.GhcRn -> PackageQualifier
determinePackageQualifier idecl =
  case CompatGHC.ideclPkgQual idecl of
    CompatGHC.NoPkgQual -> WithoutPackageQualifier
    CompatGHC.ThisPkg u -> WithPackageQualifier (T.pack $ CompatGHC.unitIdString u)
    CompatGHC.OtherPkg u -> WithPackageQualifier (T.pack $ CompatGHC.unitIdString u)

keepPackageNameOnly :: T.Text -> Maybe T.Text
keepPackageNameOnly =
  headMay . T.split (== '-')

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x
