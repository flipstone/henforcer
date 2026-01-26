{- |
Module      : TomlHelper
Description : Helpers to make dealing with toml easier.
Copyright   : (c) Flipstone Technology Partners, 2024-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module TomlHelper
  ( addField
  , setDefault
  ) where

import qualified Data.String as String
import qualified Toml

{- | A convience wrapper for 'Toml..='

@since 1.0.0.0
-}
addField ::
  String -> (object -> field) -> (Toml.Key -> Toml.TomlCodec field) -> Toml.Codec object field
addField fieldName fieldAccessor codec =
  codec (String.fromString fieldName) Toml..= fieldAccessor

{- | Set a default for what would otherwise be an optional.

@since 1.0.0.0
-}
setDefault :: a -> (Toml.Key -> Toml.TomlCodec a) -> Toml.Key -> Toml.TomlCodec a
setDefault defaultVal codec =
  let
    handleDefault Nothing = defaultVal
    handleDefault (Just x) = x
   in
    Toml.dimap pure handleDefault . Toml.dioptional . codec
