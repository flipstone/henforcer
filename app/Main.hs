module Main where

import qualified Data.Foldable as Fold
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Language.Haskell.Exts.Parser as Parse
import qualified System.Environment as Env
import qualified System.Exit as Exit

import qualified Modulint.Dependencies as Deps
import qualified Modulint.FindCycle as FindCycle
import qualified Modulint.Imports as Imports

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
            mapM_ (uncurry printCycle) (zip [1..] cycles)
            Exit.exitWith (Exit.ExitFailure 1)

printCycle :: Integer -> FindCycle.Cycle -> IO ()
printCycle cycleNum depCycle = do
  putStrLn $ ":: Cycle " ++ show cycleNum
  putStrLn $ formatCycleSummary depCycle
  putStrLn ""
  mapM_ (putStrLn . formatCyclePair) (FindCycle.cyclePairs depCycle)
  putStrLn ""

formatCycleSummary :: FindCycle.Cycle -> String
formatCycleSummary (root :| rest) =
  List.intercalate " -> " $
    map
      (Deps.formatTreeName . Deps.targetTreeName)
      (root : (rest ++ [root]))

formatCyclePair :: (Deps.TreeName, Deps.Target) -> String
formatCyclePair (source, target) =
  let
    header = Deps.formatTreeName source
              <> " depends on "
              <> Deps.formatTreeName (Deps.targetTreeName target)
              <> " due to the following:"

    causes = fmap formatImport $ Fold.toList $ Deps.targetCauses target
  in
    unlines (header : causes)

formatImport :: Imports.Import -> String
formatImport importInfo =
  let
    source = Imports.importSource importInfo
    target = Imports.importTarget importInfo
  in
    unwords
      [ " -"
      , Imports.formatModuleName source
      , "imports"
      , Imports.formatModuleName target
      , "at"
      , Imports.formatModuleNameSrcLoc target
      ]

