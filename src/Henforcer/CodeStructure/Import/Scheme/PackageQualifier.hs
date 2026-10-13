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

import qualified Data.Char as Char
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

{- | Reduce a unit id to only the package name, so that comparison against user configuration can be
written in terms of package names.

GHC gives us unit ids, which take forms such as @text-2.1.3-9f75@,
@unliftio-core-0.2.1.0-6466GKdRgYQGcOADdkqmLh@, @henforcer-1.0.0.2-inplace-henforcer-internal@, and
in principle a bare @base@. A package name may itself contain hyphens, so the leading portion cannot
simply be taken. However each hyphen separated word of a package name is required to contain a
letter, while a version is built only of digits and dots. So the package name is the longest prefix
of words that are not a version.

Note that 'Nothing' is only produced for a unit id that has no package name at all, which is not
expected to happen in practice.

@since 1.0.0.0
-}
keepPackageNameOnly :: T.Text -> Maybe T.Text
keepPackageNameOnly unitId =
  case takeWhile (not . isVersion) (T.split (== '-') unitId) of
    [] -> Nothing
    nameWords -> Just (T.intercalate (T.singleton '-') nameWords)

{- | A hyphen separated word of a unit id is a version when it is built only of digits and dots. A
word of a package name may never be such, as each is required to contain a letter.
-}
isVersion :: T.Text -> Bool
isVersion =
  T.all (\c -> Char.isDigit c || c == '.')
