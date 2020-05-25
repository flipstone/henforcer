module Main where

import qualified Data.Foldable as Fold
import qualified Language.Haskell.Exts.Parser as Parse
import qualified System.Environment as Env
import qualified System.Exit as Exit

import qualified Modulo.Dependencies as Deps
import qualified Modulo.FindCycle as FindCycle
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
        graph = Deps.buildDependencyGraph localImports
        cycles = FindCycle.findCycle graph
      in
        case length cycles of
          0 ->
            putStrLn "No cycles found!"

          count -> do
            putStrLn $ "Found " <> show count <> " cycles"
            mapM_ print $ map (map Deps.formatTreeName) cycles
            Exit.exitWith (Exit.ExitFailure 1)

