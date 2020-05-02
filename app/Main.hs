module Main where

import qualified Data.Foldable as Fold
import qualified Language.Haskell.Exts.Parser as Parse
import qualified System.Environment as Env
import qualified System.Exit as Exit

import qualified Modulo.Dependencies as Dependencies
import qualified Modulo.Imports as Imports

main :: IO ()
main = do
  directoriesToScan <- Env.getArgs
  Fold.traverse_ printDirectoryImports directoriesToScan

printDirectoryImports :: FilePath -> IO ()
printDirectoryImports directoryPath = do
  result <- Imports.loadSourceTreeImports directoryPath

  case result of
    Parse.ParseFailed srcLoc errMsg -> do
      putStrLn $ concat
        [ "Unable to load imports due to parse failure at "
        , show srcLoc
        , ": "
        , errMsg
        ]
      Exit.exitWith (Exit.ExitFailure 1)

    Parse.ParseOk allImports ->
      let
        localImports = Imports.removeNonLocalImports allImports
        graph = Dependencies.buildDependencyGraph localImports
      in
        printDependencies graph

printDependencies :: Dependencies.DependencyGraph -> IO ()
printDependencies graph =
  Fold.traverse_ printNode (Dependencies.nodes graph)
    where
      printNode node = do
        putStrLn $ concat
          [ Dependencies.formatTreeName node
          , " depends on "
          ]

        Fold.forM_ (Dependencies.dependencyTargets node graph) $ \target ->
          putStrLn $ concat [ " - ", Dependencies.formatTreeName target ]

