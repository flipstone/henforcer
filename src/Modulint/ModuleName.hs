module Modulint.ModuleName
  ( ModuleName
  , fromString
  , toString
  , fromSyntax
  , format
  ) where

import qualified Language.Haskell.Exts.Syntax as Syntax

newtype ModuleName =
  ModuleName String
  deriving (Show, Eq, Ord)

fromString :: String -> ModuleName
fromString =
  ModuleName

toString :: ModuleName -> String
toString (ModuleName moduleName) =
  moduleName

format :: ModuleName -> String
format =
  toString

fromSyntax :: Syntax.ModuleName s -> ModuleName
fromSyntax (Syntax.ModuleName _ moduleName) =
  fromString moduleName
