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
    , allowedQualifications :: Qualification.AllowedMap
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
      <$> Dhall.field (T.pack "sourcePaths")           (Dhall.list Dhall.string)
      <*> Dhall.field (T.pack "treeDependencies")      (Dhall.list dependencyDeclarationDecoder)
      <*> Dhall.field (T.pack "encapsulatedTrees")     (Dhall.list treeName)
      <*> Dhall.field (T.pack "allowedQualifications") (Dhall.map moduleName (Dhall.list allowedQualification))

dependencyDeclarationDecoder :: Dhall.Decoder DependencyDeclaration
dependencyDeclarationDecoder =
  Dhall.record $
    DependencyDeclaration
      <$> Dhall.field (T.pack "moduleTree") treeName
      <*> Dhall.field (T.pack "dependencies") (Dhall.list treeName)

allowedQualification :: Dhall.Decoder Qualification.AllowedQualification
allowedQualification =
  Dhall.record $
    Qualification.AllowedQualification
      <$> Dhall.field (T.pack "qualification") qualification
      <*> Dhall.field (T.pack "alias") alias

qualification :: Dhall.Decoder Qualification.Qualification
qualification =
  Dhall.union $
       (const Qualification.Qualified   <$> Dhall.constructor (T.pack "Qualified")     Dhall.unit)
    <> (const Qualification.Unqualified <$> Dhall.constructor (T.pack "Unqualified")   Dhall.unit)

alias :: Dhall.Decoder Qualification.Alias
alias =
  Dhall.union $
       (const Qualification.WithoutAlias <$> Dhall.constructor (T.pack "WithoutAlias")  Dhall.unit)
    <> (Qualification.WithAlias          <$> Dhall.constructor (T.pack "WithAlias")     moduleName)

moduleName :: Dhall.Decoder ModuleName.ModuleName
moduleName =
  ModuleName.fromString <$> Dhall.string

treeName :: Dhall.Decoder TreeName.TreeName
treeName =
  let
    extractTreeName input =
      case TreeName.parse input of
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

