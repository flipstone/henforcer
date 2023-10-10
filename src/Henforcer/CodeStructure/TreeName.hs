module Henforcer.CodeStructure.TreeName
  ( TreeName
  , isSuperTreeOf
  , treeContainsModule
  , treeStrictlyContainsModule
  , treeNameDecoder
  ) where

import qualified Data.Text as T
import qualified Data.Void as Void
import qualified Dhall
import qualified Dhall.Src as DhallSrc

import qualified CompatGHC

data TreeName
  = TreeName !String !(Maybe TreeName)
  deriving (Ord, Eq)

instance CompatGHC.Outputable TreeName where
  ppr =
    CompatGHC.text . format

format :: TreeName -> String
format (TreeName name maybeRest) =
  maybe name joinName maybeRest
 where
  joinName rest =
    name ++ ('.' : format rest)

parse :: String -> Either String TreeName
parse stringName =
  case maybeName of
    Just name ->
      Right name
    Nothing ->
      Left "TreeNames must not be empty!"
 where
  maybeName =
    foldr addPart Nothing (moduleNameParts stringName)

  addPart newPart subTreeName =
    Just (TreeName newPart subTreeName)

isSuperTreeOf :: TreeName -> TreeName -> Bool
isSuperTreeOf (TreeName parent mbParentRest) (TreeName child mbChildRest) =
  (parent == child)
    && case (mbParentRest, mbChildRest) of
      (Nothing, Just _) ->
        True
      (Just _, Nothing) ->
        False
      (Nothing, Nothing) ->
        True
      (Just parentRest, Just childRest) ->
        isSuperTreeOf parentRest childRest

treeContainsModule :: TreeName -> CompatGHC.ModuleName -> Bool
treeContainsModule treeName =
  isSuperTreeOf treeName . treeNameOfModule

treeStrictlyContainsModule :: TreeName -> CompatGHC.ModuleName -> Bool
treeStrictlyContainsModule treeName modName =
  let moduleTree = treeNameOfModule modName
   in (treeName /= moduleTree)
        && isSuperTreeOf treeName moduleTree

treeNameOfModule :: CompatGHC.ModuleName -> TreeName
treeNameOfModule modName =
  case parse (CompatGHC.moduleNameString modName) of
    Right name ->
      name
    Left err ->
      error $
        concat
          [ "Henforcer.CodeStructure.TreeName.treeNameOfModule: failed to parse Haskell module name "
          , CompatGHC.moduleNameString modName
          , " as a TreeName: "
          , err
          , ". This should have been impossible and probably reflects a bug in henforcer itself"
          ]

moduleNameParts :: String -> [String]
moduleNameParts modName =
  case break (== '.') modName of
    (firstPart, '.' : rest) ->
      firstPart : moduleNameParts rest
    (onlyPart, _) ->
      [onlyPart]

treeNameDecoder :: Dhall.Decoder TreeName
treeNameDecoder =
  let extractTreeName :: String -> Dhall.Extractor DhallSrc.Src Void.Void TreeName
      extractTreeName input =
        case parse input of
          Right validName ->
            pure validName
          Left err ->
            Dhall.extractError (T.pack err)
   in parseDecoder extractTreeName Dhall.string

parseDecoder ::
  (a -> Dhall.Extractor DhallSrc.Src Void.Void b)
  -> Dhall.Decoder a
  -> Dhall.Decoder b
parseDecoder parseB decoderA =
  let extractB expr =
        Dhall.fromMonadic $ do
          a <- Dhall.toMonadic (Dhall.extract decoderA expr)
          Dhall.toMonadic (parseB a)
   in decoderA
        { Dhall.extract = extractB
        }
