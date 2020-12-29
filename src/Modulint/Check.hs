module Modulint.Check
  ( CheckFailure(..)
  , CheckedDependency(..)
  , ImportChecker
  , checkImports
  , newImportChecker
  ) where

import qualified Data.Foldable as Fold
import qualified Data.STRef as STRef
import qualified Control.Monad.ST as ST

import qualified Modulint.Config as Config
import qualified Modulint.Imports as Imports
import qualified Modulint.TreeName as TreeName

data ImportChecker =
  ImportChecker
    { dependencies :: [CheckedDependency]
    }

data CheckedDependency =
  CheckedDependency
    { dependencySource :: TreeName.TreeName
    , dependencyTarget :: TreeName.TreeName
    }

data CheckFailure
  = DependencyViolation Imports.Import CheckedDependency

newImportChecker :: Config.Config -> ImportChecker
newImportChecker config =
  let
    explodeDependencies decl =
      map (\dep -> CheckedDependency (Config.moduleTree decl) dep)
      $ Config.treeDependencies decl

    configuredDependencies =
      concatMap explodeDependencies
      . Config.dependencyDeclarations
      $ config
  in
    ImportChecker
      { dependencies = configuredDependencies
      }

checkImports :: Foldable f
             => ImportChecker
             -> f Imports.Import
             -> [CheckFailure]
checkImports importChecker imports =
  ST.runST $ do
    failures <- STRef.newSTRef []
    Fold.traverse_ (checkImport failures importChecker) imports
    STRef.readSTRef failures

checkImport :: STRef.STRef s [CheckFailure]
            -> ImportChecker
            -> Imports.Import
            -> ST.ST s ()
checkImport failures checker imp =
  foldMap
    (checkImportAgainstDependency failures imp)
    (dependencies checker)

checkImportAgainstDependency :: STRef.STRef s [CheckFailure]
                             -> Imports.Import
                             -> CheckedDependency
                             -> ST.ST s ()
checkImportAgainstDependency failures imp dep = do
  let
    importSource = Imports.importSource imp
    importTarget = Imports.importTarget imp
    depSource    = dependencySource dep
    depTarget    = dependencyTarget dep

  -- If the file that contains the import belongs to the module tree that is
  -- the dependency target then we need to check wether the module being
  -- imported (the imported target) is contained in the module tree that the
  -- dependency is declared for. This is -- once a dependency is declared, we
  -- don't allow dependency targets to import modules that depend on them.
  if
    TreeName.treeContainsModule depTarget importSource &&
    TreeName.treeContainsModule depSource importTarget
  then
    STRef.modifySTRef
      failures
      (\errs -> (DependencyViolation imp dep) : errs)
  else
    pure ()
