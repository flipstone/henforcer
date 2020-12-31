module Modulint.TreeName
  ( TreeName
  , format
  , isSuperTreeOf
  , parse
  , treeContainsModule
  , treeStrictlyContainsModule
  ) where

import qualified Modulint.ModuleName as ModuleName

data TreeName =
  TreeName String (Maybe TreeName)
  deriving (Show, Ord, Eq)

format :: TreeName -> String
format (TreeName name maybeRest) =
  maybe name joinName maybeRest
    where
      joinName rest =
        name ++ ('.' : format rest)

parse :: String -> Either String TreeName
parse stringName =
  case maybeName of
    Just name ->
      Right name

    Nothing ->
      Left "TreeNames must not be empty!"

    where
      maybeName =
        foldr addPart Nothing (moduleNameParts stringName)

      addPart newPart subTreeName =
        Just (TreeName newPart subTreeName)


isSuperTreeOf :: TreeName -> TreeName -> Bool
isSuperTreeOf (TreeName parent mbParentRest) (TreeName child mbChildRest) =
  if parent /= child
  then False
  else
    case (mbParentRest, mbChildRest) of
      (Nothing, Just _) ->
        True

      (Just _, Nothing) ->
        False

      (Nothing, Nothing) ->
        True

      (Just parentRest, Just childRest) ->
        isSuperTreeOf parentRest childRest

treeContainsModule :: TreeName -> ModuleName.ModuleName -> Bool
treeContainsModule treeName moduleName =
  isSuperTreeOf treeName (treeNameOfModule moduleName)

treeStrictlyContainsModule :: TreeName -> ModuleName.ModuleName -> Bool
treeStrictlyContainsModule treeName moduleName =
  let
    moduleTree = treeNameOfModule moduleName
  in
    not (treeName == moduleTree) &&
    isSuperTreeOf treeName moduleTree

treeNameOfModule :: ModuleName.ModuleName -> TreeName
treeNameOfModule moduleName =
  case parse (ModuleName.toString moduleName) of
    Right name ->
      name

    Left err ->
      error $
        concat
          [ "Modulint.TreeName.treeNameOfModule: failed to parse Haskell module name "
          , ModuleName.format moduleName
          , " as a TreeName: "
          , err
          , ". This should have been impossible and probably reflects a bug in modulint itself"
          ]

moduleNameParts :: String -> [String]
moduleNameParts moduleName =
  case break (== '.') moduleName of
    (firstPart, '.':rest) ->
      firstPart : moduleNameParts rest

    (onlyPart, _) ->
      [onlyPart]

