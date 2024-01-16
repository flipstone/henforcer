{- |
Module      : Henforcer.Rules.MustExistNonEmpty
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.MustExistNonEmpty
  ( MustExistNonEmpty
  , mustExistNonEmptyCodec
  , checkExistsAndNonEmptyString
  ) where

import qualified Toml

newtype MustExistNonEmpty = MustExistNonEmpty Bool
  deriving (Show)

mustExistNonEmptyCodec :: Toml.Key -> Toml.TomlCodec MustExistNonEmpty
mustExistNonEmptyCodec = Toml.diwrap . Toml.bool

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
