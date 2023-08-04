{- |
Module      : Henforcer.CodeStructure.Import.Import
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.Import.Import
  ( Import (srcModule, importDecl)
  , srcLocation
  , importedModule
  , getImports
  , importIsOpenWithNoHidingOrAlias
  ) where

import qualified CompatGHC
import Henforcer.CodeStructure.Import.Scheme (Alias (WithoutAlias), Scheme (Scheme), buildScheme)

-- | `Import` is a subset of a CompatGHC.HsModule to be a slightly more ergonomic interface.
data Import = Import
  { srcModule :: CompatGHC.ModuleName -- Do not support unnamed modules just yet!
  , importDecl :: CompatGHC.LImportDecl CompatGHC.GhcRn
  }

-- | Get a 'SrcSpan' to track a location for an import
srcLocation :: Import -> CompatGHC.SrcSpan
srcLocation = CompatGHC.locA . CompatGHC.getLoc . importDecl

importedModule :: Import -> CompatGHC.ModuleName
importedModule = CompatGHC.unLoc . CompatGHC.ideclName . CompatGHC.unLoc . importDecl

-- | Get all of the 'Import's from a 'TcGblEnv'.
getImports :: CompatGHC.TcGblEnv -> [Import]
getImports tcGblEnv =
  let
    name = CompatGHC.moduleName $ CompatGHC.tcg_mod tcGblEnv
   in
    fmap (Import name) $ CompatGHC.tcg_rn_imports tcGblEnv

-- | Determine if the import is open, with no qualification, no alias, and no hiding
importIsOpenWithNoHidingOrAlias :: Import -> Bool
importIsOpenWithNoHidingOrAlias imp =
  let rawImportDecl = CompatGHC.unLoc $ importDecl imp
   in case buildScheme rawImportDecl of
        Scheme CompatGHC.NotQualified WithoutAlias _ ->
          case CompatGHC.ideclHiding rawImportDecl of
            Nothing -> True
            Just _ -> False
        _ -> False
