module Modulo.FindCycle
  ( findCycle
  ) where

import qualified Control.Monad as Monad
import qualified Control.Monad.State as ST
import qualified Data.DList as DL
import qualified Data.Set as Set

import qualified Modulo.Dependencies as Deps

type Cycle = [Deps.TreeName]

type CycleSearch a = ST.State (Set.Set Deps.TreeName) a

findCycle :: Deps.DependencyGraph -> [Cycle]
findCycle graph =
  let
    search =
      foldMapM
        (findCycleFrom graph)
        (Deps.nodes graph)
  in
    DL.toList $ ST.evalState search Set.empty

findCycleFrom :: Deps.DependencyGraph
              -> Deps.TreeName
              -> CycleSearch (DL.DList Cycle)
findCycleFrom graph =
  go []
    where
      go :: [Deps.TreeName]
         -> Deps.TreeName
         -> CycleSearch (DL.DList Cycle)
      go parentPath treeName = do
        visited <- ST.gets (Set.member treeName)
        ST.modify (Set.insert treeName)

        let
          pathToHere = (treeName : parentPath)

        if
          treeName `elem` parentPath
        then
          pure $ DL.singleton pathToHere
        else
          if
            visited
          then
            pure mempty
          else
            foldMapM
              (go pathToHere)
              (Deps.dependencyTargets treeName graph)

foldMapM :: (Monoid b, Foldable t, Monad m)
         => (a -> m b)
         -> t a
         -> m b
foldMapM f as =
  let
    accum r a = fmap (r <>) (f a)
  in
    Monad.foldM accum mempty as

