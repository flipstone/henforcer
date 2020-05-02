module Modulo.Dependencies
  ( Dependency
  , dependencySource
  , dependencyTarget
  , loadSourceTreeDependencies
  , formatModuleName
  ) where

import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Parser as Parse
import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc

import qualified Modulo.Directory as Dir

data Dependency =
  Dependency
    { dependencySource :: Syntax.ModuleName SrcLoc.SrcSpanInfo
    , dependencyTarget :: Syntax.ModuleName SrcLoc.SrcSpanInfo
    } deriving (Show, Eq, Ord)

formatModuleName :: Syntax.ModuleName a -> String
formatModuleName (Syntax.ModuleName _ moduleName) =
  moduleName

parseDependencies :: Parse.ParseMode -> String -> Parse.ParseResult (Set.Set Dependency)
parseDependencies parseMode moduleSource = do
  hsModule <- Parse.parseModuleWithMode parseMode moduleSource

  case hsModule of
    Syntax.Module _ (Just moduleHead) _ importDecls _ ->
      let
        Syntax.ModuleHead _ srcModule _ _ = moduleHead
        targetModules = map Syntax.importModule importDecls
        dependencies = map (Dependency srcModule) targetModules
      in
        pure $ Set.fromList dependencies

    Syntax.Module _ Nothing _ _ _ ->
      Parse.ParseFailed
        (SrcLoc.SrcLoc (Parse.parseFilename parseMode) 0 0)
        "Module does not yet support modules files with a module head"

    Syntax.XmlPage _ _ _ _ _ _ _ ->
      Parse.ParseFailed
        (SrcLoc.SrcLoc (Parse.parseFilename parseMode) 0 0)
        "Module does not support XmlPage modules"

    Syntax.XmlHybrid _ _ _ _ _ _ _ _ _ ->
      Parse.ParseFailed
        (SrcLoc.SrcLoc (Parse.parseFilename parseMode) 0 0)
        "Module does not support XmlHybrid modules"

loadSourceTreeDependencies :: FilePath -> IO (Parse.ParseResult (Set.Set Dependency))
loadSourceTreeDependencies baseDir =
  Dir.foldDirectory handleFile (Parse.ParseOk Set.empty) baseDir
    where
      addNewDependencies filePath accum = do
        let
          parseMode = Parse.defaultParseMode
                        { Parse.parseFilename = filePath
                        }

        parseResult <- fmap (parseDependencies parseMode) $ readFile filePath
        pure $ fmap (Set.union accum) parseResult

      -- We effectively have two monad layers to deal with here, IO and
      -- ParseResult. Hence the joining and traversing. Note that as written
      -- this will walk the _entire_ directory tree every time even after a
      -- file fails to parse. It will, however, stop reading the contents of
      -- every file in the tree.
      handleFile currentResult filePath =
        fmap Monad.join $
          traverseParseResult (addNewDependencies filePath) currentResult


-- ParseResult dosen't provide a traversable instance, so we provide our
-- own helper here to cut down on the noise in loadSourceTreeDependencies
traverseParseResult :: Applicative f
                    => (a -> f b)
                    -> Parse.ParseResult a
                    -> f (Parse.ParseResult b)
traverseParseResult f result =
  case result of
    Parse.ParseFailed srcLoc errorMessage ->
      pure $ Parse.ParseFailed srcLoc errorMessage

    Parse.ParseOk a ->
      fmap Parse.ParseOk (f a)

