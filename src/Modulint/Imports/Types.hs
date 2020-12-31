module Modulint.Imports.Types
  ( Import(..)
  , mkImport
  , formatSrcLocation
  ) where

import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import qualified Language.Haskell.Exts.Pretty as Pretty

import qualified Modulint.ModuleName as ModuleName

data Import =
  Import
    { srcModule       :: ModuleName.ModuleName
    , srcLocation     :: SrcLoc.SrcSpanInfo
    , importedModule  :: ModuleName.ModuleName
    , isQualified     :: Bool
    , alias           :: Maybe ModuleName.ModuleName
    } deriving (Show, Eq, Ord)

mkImport :: ModuleName.ModuleName
         -> Syntax.ImportDecl SrcLoc.SrcSpanInfo
         -> Import
mkImport src importDecl =
  Import
    { srcModule       = src
    , srcLocation     = Syntax.ann (Syntax.importModule importDecl)
    , importedModule  = ModuleName.fromSyntax (Syntax.importModule importDecl)
    , isQualified     = Syntax.importQualified importDecl
    , alias           = fmap ModuleName.fromSyntax (Syntax.importAs importDecl)
    }

formatSrcLocation :: SrcLoc.SrcSpanInfo -> String
formatSrcLocation =
  Pretty.prettyPrint . SrcLoc.getPointLoc
