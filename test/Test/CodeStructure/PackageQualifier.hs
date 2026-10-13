{-# LANGUAGE OverloadedStrings #-}

module Test.CodeStructure.PackageQualifier
  ( packageQualifierTests
  ) where

import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Henforcer.CodeStructure as CodeStructure

packageQualifierTests :: Tasty.TestTree
packageQualifierTests =
  Tasty.testGroup
    "Checking the package name is recovered from a unit id"
    [ recoversKnownUnitIds
    , recoversGeneratedPackageNames
    ]

{- | Unit ids as GHC actually reports them, covering hashed builds from both cabal and stack, cabal
in place builds, a sublibrary, and a version scheme that looks like a hash.
-}
knownUnitIds :: [(T.Text, Maybe T.Text)]
knownUnitIds =
  [ ("text-2.1.3-9f75", Just "text")
  , ("base-4.20.2.0-127e", Just "base")
  , ("ghc-prim-0.12.0-d0f9", Just "ghc-prim")
  , ("ghc-internal-9.1003.0-a65c", Just "ghc-internal")
  , ("unliftio-core-0.2.1.0-6466GKdRgYQGcOADdkqmLh", Just "unliftio-core")
  , ("unliftio-0.2.25.1-8b997045e030232f762ba48966a3503e", Just "unliftio")
  , ("henforcer-1.0.0.2-inplace", Just "henforcer")
  , ("henforcer-1.0.0.2-inplace-henforcer-internal", Just "henforcer")
  , ("base", Just "base")
  ]

recoversKnownUnitIds :: Tasty.TestTree
recoversKnownUnitIds =
  TastyHH.testProperty "Recovers the package name from known unit ids"
    . HH.withTests 1
    . HH.property
    $ fmap (\(unitId, _) -> (unitId, CodeStructure.keepPackageNameOnly unitId)) knownUnitIds
      HH.=== knownUnitIds

recoversGeneratedPackageNames :: Tasty.TestTree
recoversGeneratedPackageNames =
  TastyHH.testProperty "Recovers the package name from a generated name, version and hash"
    . HH.property
    $ do
      name <- HH.forAll genPackageName
      version <- HH.forAll genVersion
      hash <- HH.forAll genHash

      CodeStructure.keepPackageNameOnly (T.intercalate "-" [name, version, hash])
        HH.=== Just name

genPackageName :: HH.Gen T.Text
genPackageName =
  fmap (T.intercalate "-") (Gen.list (Range.linear 1 4) genPackageNameWord)

{- | Cabal requires every hyphen separated word of a package name to contain a letter, but places no
restriction on where in the word that letter falls.
-}
genPackageNameWord :: HH.Gen T.Text
genPackageNameWord = do
  prefix <- Gen.text (Range.linear 0 3) Gen.digit
  letter <- Gen.alpha
  suffix <- Gen.text (Range.linear 0 3) Gen.alphaNum

  pure (prefix <> T.cons letter suffix)

genVersion :: HH.Gen T.Text
genVersion =
  fmap (T.intercalate "." . fmap (T.pack . show)) (Gen.list (Range.linear 1 4) genVersionPart)

genVersionPart :: HH.Gen Int
genVersionPart =
  Gen.int (Range.linear 0 9999)

-- | A hash may be entirely digits, which must not be mistaken for the version.
genHash :: HH.Gen T.Text
genHash =
  Gen.text (Range.linear 1 16) Gen.hexit
