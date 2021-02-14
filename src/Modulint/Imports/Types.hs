module Modulint.Imports.Types
  ( Import(..)
  , mkImport
  ) where

import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc

import qualified Modulint.ModuleName as ModuleName
import qualified Modulint.Qualification as Qualification

data Import =
  Import
    { srcModule       :: ModuleName.ModuleName
    , srcLocation     :: SrcLoc.SrcSpanInfo
    , importedModule  :: ModuleName.ModuleName
    , qualification   :: Qualification.Scheme
    } deriving (Show, Eq, Ord)

mkImport :: ModuleName.ModuleName
         -> Syntax.ImportDecl SrcLoc.SrcSpanInfo
         -> Import
mkImport src importDecl =
  let
    qual =
      if
        Syntax.importQualified importDecl
      then
        Qualification.Qualified
      else
        Qualification.Unqualified

    alias =
      case Syntax.importAs importDecl of
        Nothing ->
          Qualification.WithoutAlias

        Just name ->
          Qualification.WithAlias (ModuleName.fromSyntax name)
  in
    Import
      { srcModule       = src
      , srcLocation     = Syntax.ann (Syntax.importModule importDecl)
      , importedModule  = ModuleName.fromSyntax (Syntax.importModule importDecl)
      , qualification   = Qualification.Scheme qual alias
      }

