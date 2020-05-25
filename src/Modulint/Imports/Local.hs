module Modulint.Imports.Local
  ( removeNonLocalImports
  ) where

import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Syntax as Syntax

import qualified Modulint.Imports.Types as Types

-- | removeNonLocalImports removes any import declarations which import a
-- module that is *not* found as a source of any imports in the set. This
-- allows us to remove imports of modules from libraries and such which
-- might happens to overlap with parts of the module namespace in use in
-- the project modulint is being applied to.
removeNonLocalImports :: Set.Set Types.Import -> Set.Set Types.Import
removeNonLocalImports allImports =
  let
    nameWithoutSrcInfo (Syntax.ModuleName _ name) = name
    localModules = foldMap (Set.singleton . nameWithoutSrcInfo . Types.importSource) allImports
    isLocal imp = Set.member (nameWithoutSrcInfo $ Types.importTarget imp) localModules
  in
    Set.filter isLocal allImports
