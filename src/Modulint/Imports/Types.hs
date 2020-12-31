module Modulint.Imports.Types
  ( Import(..)
  , mkImport
  , formatModuleName
  , formatModuleNameSrcLoc
  ) where

import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import qualified Language.Haskell.Exts.Pretty as Pretty

import qualified Modulint.ModuleName as ModuleName

data Import =
  Import
    { importSource    :: Syntax.ModuleName SrcLoc.SrcSpanInfo
    , importTarget    :: Syntax.ModuleName SrcLoc.SrcSpanInfo
    , importQualified :: Bool
    , importAlias     :: Maybe ModuleName.ModuleName
    } deriving (Show, Eq, Ord)

mkImport :: Syntax.ModuleName SrcLoc.SrcSpanInfo
         -> Syntax.ImportDecl SrcLoc.SrcSpanInfo
         -> Import
mkImport srcModule importDecl =
  Import
    { importSource    = srcModule
    , importTarget    = Syntax.importModule importDecl
    , importQualified = Syntax.importQualified importDecl
    , importAlias     = fmap ModuleName.syntaxToModuleName (Syntax.importAs importDecl)
    }

formatModuleName :: Syntax.ModuleName a -> String
formatModuleName (Syntax.ModuleName _ moduleName) =
  moduleName

formatModuleNameSrcLoc :: Syntax.ModuleName SrcLoc.SrcSpanInfo -> String
formatModuleNameSrcLoc (Syntax.ModuleName srcLoc _) =
  Pretty.prettyPrint $ SrcLoc.getPointLoc srcLoc
