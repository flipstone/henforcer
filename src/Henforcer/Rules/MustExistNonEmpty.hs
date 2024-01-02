{- |
Module      : Henforcer.Rules.MustExistNonEmpty
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.MustExistNonEmpty
  ( MustExistNonEmpty
  , mustExistNonEmptyDecoder
  , checkExistsAndNonEmptyString
  ) where

import qualified Dhall

newtype MustExistNonEmpty = MustExistNonEmpty Bool

mustExistNonEmptyDecoder :: Dhall.Decoder MustExistNonEmpty
mustExistNonEmptyDecoder =
  fmap MustExistNonEmpty Dhall.bool

checkExistsAndNonEmptyString ::
  MustExistNonEmpty
  -> a
  -> (a -> Maybe String)
  -> (a -> b)
  -> [b]
checkExistsAndNonEmptyString (MustExistNonEmpty bool) a getStr handleFailure =
  if bool
    then case getStr a of
      Nothing ->
        pure $ handleFailure a
      Just "" ->
        pure $ handleFailure a
      Just _ ->
        mempty
    else mempty
