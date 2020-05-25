module Modulo.Imports.Types
  ( Import(..)
  , formatModuleName
  , formatModuleNameSrcLoc
  ) where

import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import qualified Language.Haskell.Exts.Pretty as Pretty

data Import =
  Import
    { importSource :: Syntax.ModuleName SrcLoc.SrcSpanInfo
    , importTarget :: Syntax.ModuleName SrcLoc.SrcSpanInfo
    } deriving (Show, Eq, Ord)

formatModuleName :: Syntax.ModuleName a -> String
formatModuleName (Syntax.ModuleName _ moduleName) =
  moduleName

formatModuleNameSrcLoc :: Syntax.ModuleName SrcLoc.SrcSpanInfo -> String
formatModuleNameSrcLoc (Syntax.ModuleName srcLoc _) =
  Pretty.prettyPrint $ SrcLoc.getPointLoc srcLoc
