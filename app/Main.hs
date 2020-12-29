module Main where

import qualified Data.Foldable as Fold
import qualified Language.Haskell.Exts.Parser as Parse
import qualified System.Exit as Exit

import qualified Modulint.Config as Config
import qualified Modulint.Check as Check
import qualified Modulint.Imports as Imports
import qualified Modulint.Options as Options
import qualified Modulint.TreeName as TreeName

main :: IO ()
main = do
  options <- Options.parseOptions
  config <- Config.loadConfigFile (Options.configPath options)

  let
    importChecker = Check.newImportChecker config

  Fold.traverse_
    (printDirectoryImports importChecker)
    (Options.sourcePaths options)

printDirectoryImports :: Check.ImportChecker -> FilePath -> IO ()
printDirectoryImports importChecker directoryPath = do
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

    Parse.ParseOk allImports -> do
      let
        localImports  = Imports.removeNonLocalImports allImports
        checkFailures = Check.checkImports importChecker localImports

      Fold.traverse_ showCheckFailure checkFailures

      case length checkFailures of
        0 -> do
          putStrLn "modulint: All checks passed!"
          Exit.exitWith Exit.ExitSuccess

        errCount -> do
          putStrLn $ "\nmodulint: " <> show errCount <> " errors found!"
          Exit.exitWith (Exit.ExitFailure 1)


showCheckFailure :: Check.CheckFailure -> IO ()
showCheckFailure result =
  case result of
    Check.DependencyViolation imp dep ->
      let
        depSource = Check.dependencySource dep
        depTarget = Check.dependencyTarget dep
        impSource = Imports.importSource imp
        impTarget = Imports.importTarget imp
      in
        putStrLn $ unwords
          [ "The import of"
          , Imports.formatModuleName impTarget
          , "by"
          , Imports.formatModuleName impSource
          , "at"
          , Imports.formatModuleNameSrcLoc impTarget
          , "is forbidden by the declaration that the module tree"
          , TreeName.formatTreeName depSource
          , "depends on"
          , TreeName.formatTreeName depTarget
          ]
