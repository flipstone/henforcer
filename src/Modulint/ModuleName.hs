module Modulint.ModuleName
  ( ModuleName
  , stringToModuleName
  , syntaxToModuleName
  , formatModuleName
  ) where

import qualified Language.Haskell.Exts.Syntax as Syntax

newtype ModuleName =
  ModuleName String
  deriving (Show, Eq, Ord)

stringToModuleName :: String -> ModuleName
stringToModuleName =
  ModuleName

formatModuleName :: ModuleName -> String
formatModuleName (ModuleName moduleName) =
  moduleName

syntaxToModuleName :: Syntax.ModuleName s -> ModuleName
syntaxToModuleName (Syntax.ModuleName _ moduleName) =
  stringToModuleName moduleName
