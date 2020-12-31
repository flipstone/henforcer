module Modulint.Config
  ( Config(..)
  , DependencyDeclaration(..)
  , loadConfigFile
  ) where

import qualified Data.Text as T
import qualified Data.Void as Void
import qualified Dhall
import qualified Dhall.Src as DhallSrc
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import qualified Modulint.ModuleName as ModuleName
import qualified Modulint.TreeName as TreeName
import qualified Modulint.Qualification as Qualification

data Config =
  Config
    { sourcePaths :: [FilePath]
    , dependencyDeclarations :: [DependencyDeclaration]
    , encapsulatedTrees :: [TreeName.TreeName]
    , qualificationRules :: Qualification.RuleMap
    } deriving (Show)

data DependencyDeclaration =
  DependencyDeclaration
    { moduleTree :: TreeName.TreeName
    , treeDependencies :: [TreeName.TreeName]
    } deriving (Show)

loadConfigFile :: FilePath -> IO Config
loadConfigFile configPath = do
  rawConfig <- Dhall.inputFile configDecoder configPath
  pure $
    rawConfig
      { sourcePaths = map (relativize configPath) (sourcePaths rawConfig)
      }

relativize :: FilePath -> FilePath -> FilePath
relativize configPath sourcePath =
  FilePath.takeDirectory configPath </> sourcePath

configDecoder :: Dhall.Decoder Config
configDecoder =
  Dhall.record $
    Config
      <$> Dhall.field (T.pack "sourcePaths")        (Dhall.list Dhall.string)
      <*> Dhall.field (T.pack "treeDependencies")   (Dhall.list dependencyDeclarationDecoder)
      <*> Dhall.field (T.pack "encapsulatedTrees")  (Dhall.list treeName)
      <*> Dhall.field (T.pack "qualificationRules") (Dhall.map moduleName qualificationRule)

dependencyDeclarationDecoder :: Dhall.Decoder DependencyDeclaration
dependencyDeclarationDecoder =
  Dhall.record $
    DependencyDeclaration
      <$> Dhall.field (T.pack "moduleTree") treeName
      <*> Dhall.field (T.pack "dependencies") (Dhall.list treeName)

qualificationRule :: Dhall.Decoder Qualification.Rule
qualificationRule =
  Dhall.union $
       (const Qualification.Forbidden <$> Dhall.constructor (T.pack "Forbidden")    Dhall.unit)
    <> (Qualification.RequiredAs      <$> Dhall.constructor (T.pack "RequiredAs") (Dhall.list moduleName))
    <> (Qualification.AllowedAs       <$> Dhall.constructor (T.pack "AllowedAs")   (Dhall.list moduleName))

moduleName :: Dhall.Decoder ModuleName.ModuleName
moduleName =
  ModuleName.fromString <$> Dhall.string

treeName :: Dhall.Decoder TreeName.TreeName
treeName =
  let
    extractTreeName input =
      case TreeName.parseTreeName input of
        Right validName ->
          pure validName

        Left err ->
          Dhall.extractError (T.pack err)
  in
    parseDecoder extractTreeName Dhall.string

parseDecoder :: (a -> Dhall.Extractor DhallSrc.Src Void.Void b)
             -> Dhall.Decoder a
             -> Dhall.Decoder b
parseDecoder parseB decoderA =
  let
    extractB expr =
      Dhall.fromMonadic $ do
        a <- Dhall.toMonadic (Dhall.extract decoderA expr)
        Dhall.toMonadic (parseB a)

  in
    decoderA
      { Dhall.extract = extractB
      }

