{- |
Module      : Henforcer.Config.TreeDependencies
Description : Functionality for specifying module trees that depend on other module trees.
Copyright   : (c) Flipstone Technology Partners, 2024
License     : BSD-3-clause
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
-}
data TreeDependency = TreeDependency
  { moduleTree :: CodeStructure.TreeName
  , treeDependencies :: [CodeStructure.TreeName]
  , treeDependencyNote :: Rules.UserNote
  }

treeDependencyCodec :: Toml.TomlCodec TreeDependency
treeDependencyCodec =
  TreeDependency
    <$> TomlHelper.addField "moduleTree" moduleTree CodeStructure.treeNameCodec
    <*> TomlHelper.addField "dependencies" treeDependencies CodeStructure.treeNameListCodec
    <*> Rules.userNoteField treeDependencyNote
