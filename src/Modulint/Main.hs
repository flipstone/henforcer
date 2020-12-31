module Modulint.Main
  ( main
  , checksFailed
  ) where

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Language.Haskell.Exts.Parser as Parse
import qualified System.Exit as Exit

import qualified Modulint.Config as Config
import qualified Modulint.Check as Check
import qualified Modulint.Imports as Imports
import qualified Modulint.Initialize as Initialize
import qualified Modulint.ModuleName as ModuleName
import qualified Modulint.Options as Options
import qualified Modulint.TreeName as TreeName

main :: Options.Options -> IO ()
main options = do
  if
    Options.initialize options
  then
    Initialize.initializeConfigPath (Options.configPath options)
  else
    run options

run :: Options.Options -> IO ()
run options = do
  config <- Config.loadConfigFile (Options.configPath options)

  let
    importChecker = Check.newImportChecker config

  Fold.traverse_
    (printDirectoryImports importChecker)
    (Config.sourcePaths config)

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
      Exit.exitWith moduleParseFailure

    Parse.ParseOk allImports -> do
      let
        checkFailures =
          Check.checkImports importChecker allImports

        errorOutput =
          List.intercalate "\n\n" (map formatCheckFailure checkFailures)

      case length checkFailures of
        0 -> do
          putStrLn "modulint: All checks passed!"
          Exit.exitWith Exit.ExitSuccess

        errCount -> do
          putStrLn errorOutput
          putStrLn $ "\nmodulint: " <> show errCount <> " errors found!"
          Exit.exitWith checksFailed

formatCheckFailure :: Check.CheckFailure -> String
formatCheckFailure failure =
  case failure of
    Check.DependencyViolation imp dep ->
      formatDependencyViolation imp dep

    Check.EncapsulationViolation imp treeName ->
      formatEncapsulationViolation imp treeName

    Check.QualificationViolation imp violation ->
      formatQualificationViolation imp violation

formatDependencyViolation :: Imports.Import -> Check.CheckedDependency -> String
formatDependencyViolation imp dep =
  let
    depSource = Check.dependencySource dep
    depTarget = Check.dependencyTarget dep
  in
    unwords
      [ formatImportSubject imp
      , "is forbidden by the declaration that the module tree"
      , TreeName.format depSource
      , "depends on"
      , TreeName.format depTarget
      ]

formatEncapsulationViolation :: Imports.Import -> TreeName.TreeName -> String
formatEncapsulationViolation imp treeName =
  unwords
    [ formatImportSubject imp
    , "is forbidden because it is an internal module of the encapsulated tree"
    , TreeName.format treeName
    ]

formatQualificationViolation :: Imports.Import -> Check.QualificationViolation -> String
formatQualificationViolation imp violation =
  case violation of
    Check.QualificationForbidden ->
      unwords
        [ formatImportSubject imp
        , "is improper because it is qualified. Qualified imports"
        , "of this module have been declared forbidden."
        ]

    Check.QualificationRequired allowedAliases  ->
      unwords
        ( formatImportSubject imp
        : "is improper because it is not qualified. =mports"
        : "of this module are declared to require qualification"
        : "using one of the following aliases:"
        : map ModuleName.format allowedAliases
        )

    Check.DisallowedAlias badAlias allowedAliases  ->
      unwords
        ( formatImportSubject imp
        : "is improper because the alias"
        : ModuleName.format badAlias
        : "is not one of the allowed aliases. The allowed aliases are:"
        : map ModuleName.format allowedAliases
        )

formatImportSubject :: Imports.Import -> String
formatImportSubject imp =
  unwords
    [ "The import of"
    , ModuleName.format (Imports.importedModule imp)
    , "by"
    , ModuleName.format (Imports.srcModule imp)
    , "at"
    , Imports.formatSrcLocation (Imports.srcLocation imp)
    ]

moduleParseFailure :: Exit.ExitCode
moduleParseFailure =
  Exit.ExitFailure 10

checksFailed :: Exit.ExitCode
checksFailed =
  Exit.ExitFailure 20
