{- |
Module      : Henforcer.Config.TreeDependencies
Description : Functionality for specifying module trees that depend on other module trees.
Copyright   : (c) Flipstone Technology Partners, 2024-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config.TreeDependencies
  ( TreeDependency (..)
  , treeDependencyCodec
  ) where

import qualified Toml

import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Rules as Rules
import qualified TomlHelper

{- | Type to allow us to specify when a subtree depends on other subtrees. Particularly this allows
for a relatively compact expression in the user facing TOML. This expression is why the module is
placed under 'Henforcer.Config' and not elsewhere such as 'Henforcer.Rules'.

@since 1.0.0.0
-}
data TreeDependency = TreeDependency
  { moduleTree :: !CodeStructure.ModuleTree
  , treeDependencies :: ![CodeStructure.ModuleTree]
  , treeDependencyNote :: !Rules.UserNote
  }

treeDependencyCodec :: Toml.TomlCodec TreeDependency
treeDependencyCodec =
  TreeDependency
    <$> TomlHelper.addField "moduleTree" moduleTree CodeStructure.moduleTreeCodec
    <*> TomlHelper.addField "dependencies" treeDependencies CodeStructure.moduleTreeListCodec
    <*> Rules.userNoteField treeDependencyNote
