module Modulo.FindCycle
  ( findCycle
  , cyclePairs
  , Cycle
  ) where

import qualified Control.Monad as Monad
import qualified Control.Monad.State as ST
import qualified Data.DList as DL
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set

import qualified Modulo.Dependencies as Deps

type Cycle = NonEmpty.NonEmpty Deps.Target

cyclePairs :: Cycle -> [(Deps.TreeName, Deps.Target)]
cyclePairs (root :| rest) =
  go DL.empty root rest
    where
      go result prev remaining =
        case remaining of
          [] ->
            let
              pair = (Deps.targetTreeName prev, root)
            in
              DL.toList (DL.snoc result pair)

          (next : remainingRest) ->
            let
              pair = (Deps.targetTreeName prev, next)
            in
              go (DL.snoc result pair) next remainingRest

data CycleInProgress
  = Start Deps.TreeName
  | ImportTarget Deps.Target CycleInProgress

checkCycle :: Deps.Target -> CycleInProgress -> Maybe Cycle
checkCycle newTarget =
  go (newTarget :| [])
  where
    newName = Deps.targetTreeName newTarget

    go possibleCycle inProgress =
      case inProgress of
        Start name
          | name == newName ->
            Just possibleCycle

          | otherwise ->
            Nothing

        ImportTarget target previousProgress
          | Deps.targetTreeName target == newName ->
            Just possibleCycle

          | otherwise ->
            go (NonEmpty.cons target possibleCycle) previousProgress


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

data VisitationState
  = PreviouslyVisited
  | NewlyVisited

checkAndMarkedVisited :: Deps.TreeName -> CycleSearch VisitationState
checkAndMarkedVisited treeName = do
  previouslyVisited <- ST.gets (Set.member treeName)
  ST.modify (Set.insert treeName)

  pure $
    if
      previouslyVisited
    then
      PreviouslyVisited
    else
      NewlyVisited

findCycleFrom :: Deps.DependencyGraph
              -> Deps.TreeName
              -> CycleSearch (DL.DList Cycle)
findCycleFrom graph =
  start
    where
      start :: Deps.TreeName -> CycleSearch (DL.DList Cycle)
      start treeName = do
        visitation <- checkAndMarkedVisited treeName
        continue
          visitation
          treeName
          (Start treeName)

      visitTarget :: CycleInProgress
                  -> Deps.Target
                  -> CycleSearch (DL.DList Cycle)
      visitTarget parentPath target = do
        let
          treeName = Deps.targetTreeName target

        visitation <- checkAndMarkedVisited treeName

        case checkCycle target parentPath of
          Just foundCycle ->
            pure $ DL.singleton foundCycle

          Nothing ->
            continue
              visitation
              treeName
              (ImportTarget target parentPath)

      continue visitation treeName pathToHere =
        case visitation of
          PreviouslyVisited ->
            pure mempty

          NewlyVisited ->
            foldMapM
              (visitTarget pathToHere)
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

