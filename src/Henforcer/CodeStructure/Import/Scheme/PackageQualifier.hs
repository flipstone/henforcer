{- |
Module      : Henforcer.CodeStructure.Import.Scheme.PackageQualified
Description : Models the representation of a package qualifier portion of an import statement.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Scheme.PackageQualifier
  ( PackageQualifier (..)
  , packageQualifierDecoder
  , determinePackageQualifier
  , keepPackageNameOnly
  ) where

import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC

-- | Directly represent if an import has a package qualifier or not.
data PackageQualifier
  = WithPackageQualifier T.Text
  | WithoutPackageQualifier
  deriving (Eq)

-- | Compute the packageQualifier from an import
determinePackageQualifier :: CompatGHC.ImportDecl CompatGHC.GhcRn -> PackageQualifier
determinePackageQualifier idecl =
  case CompatGHC.ideclPkgQual idecl of
    CompatGHC.NotPkgQualified -> WithoutPackageQualifier
    CompatGHC.PkgQualified s -> WithPackageQualifier (T.pack s)

-- | Dhall decoder for 'PackageQualifier' so that it can be read from configuration
packageQualifierDecoder :: Dhall.Decoder PackageQualifier
packageQualifierDecoder =
  Dhall.union $
    (const WithoutPackageQualifier <$> Dhall.constructor (T.pack "WithoutPackageQualifier") Dhall.unit)
      <> ( WithPackageQualifier
            <$> Dhall.constructor (T.pack "WithPackageQualifier") (fmap T.pack Dhall.string)
         )

keepPackageNameOnly :: T.Text -> Maybe T.Text
keepPackageNameOnly =
  headMay . T.split (== '-')

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x
