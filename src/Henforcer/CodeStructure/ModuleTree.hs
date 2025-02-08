{- |
Module      : Henforcer.CodeStructure.ModuleTree
Description : Definition of 'ModuleTree', parsing, and related functionality.
Copyright   : (c) Flipstone Technology Partners, 2023-2025
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.CodeStructure.ModuleTree
  ( ModuleTree
  , parse
  , isSuperTreeOf
  , treeContainsModule
  , treeStrictlyContainsModule
  , moduleTreeCodec
  , moduleTreeListCodec
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Toml

import qualified CompatGHC

{- | Conceptually a "module tree" refers to a root module (e.g. `Data.Text`) and all the modules are
prefixed by it (e.g. `Data.Text.Encoding` and `Data.Text.Lazy`). A 'ModuleTree' is a
representation of a 'CompatGHC.ModuleName'
@since 1.0.0.0
-}
data ModuleTree
  = ModuleTree !String !(Maybe ModuleTree)
  deriving (Eq, Show)

-- | @since 1.0.0.0
instance CompatGHC.Outputable ModuleTree where
  ppr =
    CompatGHC.text . format

{- | Invert 'parse', using "." as the separator between parts of the 'ModuleTree'.

@since 1.0.0.0
-}
format :: ModuleTree -> String
format (ModuleTree name maybeRest) =
  let
    joinName rest =
      name <> ('.' : format rest)
   in
    maybe name joinName maybeRest

{- | Parse a 'String' into a 'ModuleTree', the input will not be validated.

@since 1.0.0.0
-}
parse :: String -> ModuleTree
parse str =
  let
    addPart newPart subModuleTree =
      Just (ModuleTree newPart subModuleTree)

    (x NEL.:| xs) = moduleNameParts str
   in
    ModuleTree x $ foldr addPart Nothing xs

{- | Is a 'ModuleTree' contained within another 'ModuleTree'.

@since 1.0.0.0
-}
isSuperTreeOf :: ModuleTree -> ModuleTree -> Bool
isSuperTreeOf (ModuleTree parent mbParentRest) (ModuleTree child mbChildRest) =
  (parent == child)
    && case (mbParentRest, mbChildRest) of
      (Nothing, Just _) ->
        True
      (Just _, Nothing) ->
        False
      (Nothing, Nothing) ->
        True
      (Just parentRest, Just childRest) ->
        isSuperTreeOf parentRest childRest

{- | Is a 'CompatGHC.ModuleName' contained within a 'ModuleTree'.

@since 1.0.0.0
-}
treeContainsModule :: ModuleTree -> CompatGHC.ModuleName -> Bool
treeContainsModule moduleTree =
  isSuperTreeOf moduleTree . moduleTreeOfModule

{- | Is a 'CompatGHC.ModuleName' contained within a 'ModuleTree' without being exactly the
'ModuleTree'.

@since 1.0.0.0
-}
treeStrictlyContainsModule :: ModuleTree -> CompatGHC.ModuleName -> Bool
treeStrictlyContainsModule moduleTree modName =
  let moduleTreeForName = moduleTreeOfModule modName
   in (moduleTree /= moduleTreeForName)
        && isSuperTreeOf moduleTree moduleTreeForName

{- | Parse a 'CompatGHC.ModuleName' into a 'ModuleTree'.

@since 1.0.0.0
-}
moduleTreeOfModule :: CompatGHC.ModuleName -> ModuleTree
moduleTreeOfModule =
  parse . CompatGHC.moduleNameString

moduleNameParts :: String -> NEL.NonEmpty String
moduleNameParts modName =
  case break (== '.') modName of
    (firstPart, '.' : rest) ->
      NEL.cons firstPart $ moduleNameParts rest
    (onlyPart, _) ->
      pure onlyPart

{- | A codec for a single 'ModuleTree'

@since 1.0.0.0
-}
moduleTreeCodec :: Toml.Key -> Toml.TomlCodec ModuleTree
moduleTreeCodec =
  Toml.dimap format parse . Toml.string

{- | A codec for a list of 'ModuleTree' that is required and if missing will throw as opposed to
being an empty list.

@since 1.0.0.0
-}
moduleTreeListCodec :: Toml.Key -> Toml.TomlCodec [ModuleTree]
moduleTreeListCodec =
  Toml.arrayOf (Toml._TextBy (T.pack . format) (pure . parse . T.unpack))
