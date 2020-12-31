module Modulint.Imports.Load
  ( loadSourceTreeImports
  ) where

import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified Language.Haskell.Exts.Parser as Parse
import qualified Language.Haskell.Exts.Syntax as Syntax
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc
import qualified System.FilePath as FilePath

import qualified Modulint.Directory as Dir
import qualified Modulint.Imports.Types as Types

parseImports :: Parse.ParseMode -> String -> Parse.ParseResult (Set.Set Types.Import)
parseImports parseMode moduleSource = do
  moduleHeadAndImports <- Parse.parseWithMode parseMode moduleSource

  case moduleHeadAndImports of
    Parse.NonGreedy (Parse.ModuleHeadAndImports srcInfo _ maybeHead importDecls) ->
      case maybeHead of
        Nothing ->
          Parse.ParseFailed
          (SrcLoc.fromSrcInfo srcInfo)
          "Modulint does not yet support modules files without a module head"

        Just (Syntax.ModuleHead _ srcModule _ _) ->
          let
            imports = map (Types.mkImport srcModule) importDecls
          in
            pure $ Set.fromList imports

isHaskellFile :: FilePath -> Bool
isHaskellFile filePath =
  FilePath.takeExtension filePath == ".hs"

loadSourceTreeImports :: FilePath -> IO (Parse.ParseResult (Set.Set Types.Import))
loadSourceTreeImports baseDir =
  Dir.foldDirectory isHaskellFile handleFile (Parse.ParseOk Set.empty) baseDir
    where
      addNewImports filePath accum = do
        let
          parseMode = Parse.defaultParseMode
                        { Parse.parseFilename = filePath
                        }

        parseResult <- fmap (parseImports parseMode) $ readFile filePath
        pure $ fmap (Set.union accum) parseResult

      -- We effectively have two monad layers to deal with here, IO and
      -- ParseResult. Hence the joining and traversing. Note that as written
      -- this will walk the _entire_ directory tree every time even after a
      -- file fails to parse. It will, however, stop reading the contents of
      -- every file in the tree.
      handleFile currentResult filePath =
        fmap Monad.join $
          traverseParseResult (addNewImports filePath) currentResult


-- ParseResult dosen't provide a traversable instance, so we provide our
-- own helper here to cut down on the noise in loadSourceTreeImports
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

