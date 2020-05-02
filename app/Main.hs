module Main where

import qualified Data.Foldable as Fold
import qualified Language.Haskell.Exts.Parser as Parse
import qualified System.Environment as Env
import qualified System.Exit as Exit

import qualified Modulo.Dependencies as Deps

main :: IO ()
main = do
  directoriesToScan <- Env.getArgs
  Fold.traverse_ printDirectoryDeps directoriesToScan

printDirectoryDeps :: FilePath -> IO ()
printDirectoryDeps directoryPath = do
  result <- Deps.loadSourceTreeDependencies directoryPath

  case result of
    Parse.ParseFailed srcLoc errMsg -> do
      putStrLn $ concat
        [ "Unable to load dependencies due to parse failure at "
        , show srcLoc
        , ": "
        , errMsg
        ]
      Exit.exitWith (Exit.ExitFailure 1)

    Parse.ParseOk deps ->
      Fold.traverse_ printDependency deps

printDependency :: Deps.Dependency -> IO ()
printDependency dependency =
  putStrLn $ concat
    [ Deps.formatModuleName (Deps.dependencySource dependency)
    , " depends on "
    , Deps.formatModuleName (Deps.dependencyTarget dependency)
    ]

