module Modulo.Imports.Types
  ( Import(..)
  , formatModuleName
  ) where

import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc

data Import =
  Import
    { importSource :: Syntax.ModuleName SrcLoc.SrcSpanInfo
    , importTarget :: Syntax.ModuleName SrcLoc.SrcSpanInfo
    } deriving (Show, Eq, Ord)

formatModuleName :: Syntax.ModuleName a -> String
formatModuleName (Syntax.ModuleName _ moduleName) =
  moduleName
