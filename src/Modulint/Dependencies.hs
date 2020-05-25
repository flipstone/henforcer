module Modulint.Dependencies
  ( DependencyGraph
  , buildDependencyGraph
  , TreeName
  , formatTreeName
  , treeNameDepth
  , nodes
  , Target
  , targetTreeName
  , targetCauses
  , dependencyTargets
  ) where

import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Numeric.Natural as Nat

import qualified Modulint.Imports as Imports

data TreeName =
  TreeName String (Maybe TreeName)
  deriving (Show, Ord, Eq)

formatTreeName :: TreeName -> String
formatTreeName (TreeName name maybeRest) =
  maybe name joinName maybeRest
    where
      joinName rest =
        name ++ ('.' : formatTreeName rest)

treeNameDepth :: TreeName -> Nat.Natural
treeNameDepth =
  go 1
    where
      go depth (TreeName _ child) =
        case child of
          Nothing ->
            depth

          Just treeName ->
            go (depth + 1) treeName

newtype Targets =
  Targets (Map.Map TreeName (Set.Set Imports.Import))
  deriving (Show)

instance Semigroup Targets where
  (Targets t1) <> (Targets t2) =
    Targets (Map.unionWith (<>) t1 t2)

newtype DependencyGraph =
  DependencyGraph (Map.Map TreeName Targets)
  deriving (Show)

instance Semigroup DependencyGraph where
  (DependencyGraph g1) <> (DependencyGraph g2) =
    DependencyGraph (Map.unionWith (<>) g1 g2)

instance Monoid DependencyGraph where
  mempty =
    DependencyGraph mempty

data Target =
  Target TreeName (Set.Set Imports.Import)
  deriving (Show, Eq, Ord)

targetTreeName :: Target -> TreeName
targetTreeName (Target treeName _) =
  treeName

targetCauses :: Target -> Set.Set Imports.Import
targetCauses (Target _ causes) =
  causes

nodes :: DependencyGraph -> Set.Set TreeName
nodes (DependencyGraph graph) =
  Set.fromList $ Map.keys graph

dependencyTargets :: TreeName -> DependencyGraph -> [Target]
dependencyTargets name (DependencyGraph graph) =
  targetsToList $
  Map.findWithDefault
    (Targets Map.empty)
    name
    graph

targetsToList :: Targets -> [Target]
targetsToList (Targets targetMap) =
  fmap (uncurry Target) $ Map.toList $ targetMap


buildDependencyGraph :: Set.Set Imports.Import -> DependencyGraph
buildDependencyGraph =
  foldMap mkImportDependencies

singletonGraph :: TreeName -> TreeName -> Imports.Import -> DependencyGraph
singletonGraph source target cause =
  DependencyGraph $
    Map.singleton source $
      Targets (Map.singleton target (Set.singleton cause))

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

mkImportDependencies :: Imports.Import -> DependencyGraph
mkImportDependencies imp =
  mconcat $ do
    sourceTree <- treeNamesForModule (Imports.importSource imp)
    targetTree <- treeNamesForModule (Imports.importTarget imp)

    -- A.B.C should be allowed to import A.B.D without that import causing
    -- an implied dependency from A.B.C to A.B, so we filter out any cases
    -- where the target tree is a parent of the source tree. This will actually
    -- filter out cases where the dependency was explicitly rather than implied
    -- (i.e. when A.B.C actually imports A.B), which I've noted in the README
    -- as a known issue for us to fix later.
    Monad.guard $ not $ isSuperTreeOf targetTree sourceTree

    pure $
      singletonGraph sourceTree targetTree imp

treeNamesForModule :: Syntax.ModuleName a -> [TreeName]
treeNamesForModule (Syntax.ModuleName _ name) =
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

