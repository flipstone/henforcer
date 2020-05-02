module Main where

import qualified Data.Foldable as Fold
import qualified Language.Haskell.Exts.Parser as Parse
import qualified System.Environment as Env
import qualified System.Exit as Exit

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
      in
        Fold.traverse_ printImport localImports

printImport :: Imports.Import -> IO ()
printImport imp =
  putStrLn $ concat
    [ Imports.formatModuleName (Imports.importSource imp)
    , " imports "
    , Imports.formatModuleName (Imports.importTarget imp)
    ]

