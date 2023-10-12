{- |
Module      : Henforcer.CodeStructure.Import.AllowedAliasUniqueness
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.AllowedAliasUniqueness
  ( AllowedAliasUniqueness (..)
  , allowedAliasUniquenessDecoder
  ) where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Dhall

import qualified CompatGHC

data AllowedAliasUniqueness
  = AllAliasesUniqueExcept !(Set.Set CompatGHC.ModuleName)
  | AliasesToBeUnique !(Set.Set CompatGHC.ModuleName)
  | NoAliasUniqueness

allowedAliasUniquenessDecoder :: Dhall.Decoder AllowedAliasUniqueness
allowedAliasUniquenessDecoder =
  Dhall.union $
    fmap
      AllAliasesUniqueExcept
      ( Dhall.constructor
          (T.pack "AllAliasesUniqueExcept")
          (Dhall.setIgnoringDuplicates CompatGHC.moduleNameDecoder)
      )
      <> fmap
        AliasesToBeUnique
        ( Dhall.constructor
            (T.pack "AliasesToBeUnique")
            (Dhall.setIgnoringDuplicates CompatGHC.moduleNameDecoder)
        )
      <> fmap (const NoAliasUniqueness) (Dhall.constructor (T.pack "NoAliasUniqueness") Dhall.unit)
