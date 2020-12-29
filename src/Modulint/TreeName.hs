module Modulint.TreeName
  ( TreeName
  , formatTreeName
  , isSuperTreeOf
  , treeNamesForModule
  , parseTreeName
  , treeContainsModule
  ) where

import qualified Language.Haskell.Exts.Syntax as Syntax

data TreeName =
  TreeName String (Maybe TreeName)
  deriving (Show, Ord, Eq)

formatTreeName :: TreeName -> String
formatTreeName (TreeName name maybeRest) =
  maybe name joinName maybeRest
    where
      joinName rest =
        name ++ ('.' : formatTreeName rest)

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

treeContainsModule :: TreeName -> Syntax.ModuleName s -> Bool
treeContainsModule treeName moduleName =
  isSuperTreeOf treeName (treeNameOfModule moduleName)

treeNameOfModule :: Syntax.ModuleName s -> TreeName
treeNameOfModule (Syntax.ModuleName _ stringName) =
  case parseTreeName stringName of
    Right name ->
      name

    Left err ->
      error $
        concat
          [ "Modulint.TreeName.treeNameOfModule: failed to parse Haskell module name "
          , show stringName
          , " as a TreeName: "
          , err
          , ". This should have been impossible and probably reflects a bug in modulint itself"
          ]

parseTreeName :: String -> Either String TreeName
parseTreeName stringName =
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

treeNamesForModule :: String -> [TreeName]
treeNamesForModule name =
  go $ moduleNameParts name
    where
      go parts =
        case parts of
          [] ->
            []

          (firstPart : restParts) ->
            let
              standaloneName = TreeName firstPart Nothing
              prependName = TreeName firstPart . Just
            in
              standaloneName : (map prependName (go restParts))

moduleNameParts :: String -> [String]
moduleNameParts moduleName =
  case break (== '.') moduleName of
    (firstPart, '.':rest) ->
      firstPart : moduleNameParts rest

    (onlyPart, _) ->
      [onlyPart]

