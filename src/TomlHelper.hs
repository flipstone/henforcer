{- |
Module      : TomlHelper
Description : Helpers to make dealing with toml easier.
Copyright   : (c) Flipstone Technology Partners, 2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module TomlHelper
  ( addField
  , setDefault
  ) where

import qualified Data.String as String
import qualified Toml

addField ::
  String -> (object -> field) -> (Toml.Key -> Toml.TomlCodec field) -> Toml.Codec object field
addField fieldName fieldAccessor codec =
  codec (String.fromString fieldName) Toml..= fieldAccessor

setDefault :: a -> (Toml.Key -> Toml.TomlCodec a) -> Toml.Key -> Toml.TomlCodec a
setDefault defaultVal codec =
  let
    handleDefault Nothing = defaultVal
    handleDefault (Just x) = x
   in
    Toml.dimap pure handleDefault . Toml.dioptional . codec
