module Henforcer.CodeStructure.TreeName
  ( TreeName
  , parse
  , isSuperTreeOf
  , treeContainsModule
  , treeStrictlyContainsModule
  , treeNameCodec
  , treeNameListCodec
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Toml

import qualified CompatGHC

data TreeName
  = TreeName !String !(Maybe TreeName)
  deriving (Eq, Show)

instance CompatGHC.Outputable TreeName where
  ppr =
    CompatGHC.text . format

format :: TreeName -> String
format (TreeName name maybeRest) =
  maybe name joinName maybeRest
 where
  joinName rest =
    name ++ ('.' : format rest)

parse :: String -> TreeName
parse str =
  let
    addPart newPart subTreeName =
      Just (TreeName newPart subTreeName)

    (x NEL.:| xs) = moduleNameParts str
   in
    TreeName x $ foldr addPart Nothing xs

isSuperTreeOf :: TreeName -> TreeName -> Bool
isSuperTreeOf (TreeName parent mbParentRest) (TreeName child mbChildRest) =
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

treeContainsModule :: TreeName -> CompatGHC.ModuleName -> Bool
treeContainsModule treeName =
  isSuperTreeOf treeName . treeNameOfModule

treeStrictlyContainsModule :: TreeName -> CompatGHC.ModuleName -> Bool
treeStrictlyContainsModule treeName modName =
  let moduleTree = treeNameOfModule modName
   in (treeName /= moduleTree)
        && isSuperTreeOf treeName moduleTree

treeNameOfModule :: CompatGHC.ModuleName -> TreeName
treeNameOfModule =
  parse . CompatGHC.moduleNameString

moduleNameParts :: String -> NEL.NonEmpty String
moduleNameParts modName =
  case break (== '.') modName of
    (firstPart, '.' : rest) ->
      NEL.cons firstPart $ moduleNameParts rest
    (onlyPart, _) ->
      pure onlyPart

treeNameCodec :: Toml.Key -> Toml.TomlCodec TreeName
treeNameCodec =
  Toml.dimap format parse . Toml.string

treeNameListCodec :: Toml.Key -> Toml.TomlCodec [TreeName]
treeNameListCodec =
  Toml.arrayOf (Toml._TextBy (T.pack . format) (pure . parse . T.unpack))
