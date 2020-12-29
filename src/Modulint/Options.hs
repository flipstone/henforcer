module Modulint.Options
  ( Options(..)
  , parseOptions
  ) where

import qualified Options.Applicative as Opt

data Options =
  Options
    { configPath :: FilePath
    , sourcePaths :: [FilePath]
    } deriving (Show)

parseOptions :: IO Options
parseOptions =
  Opt.execParser optionsInfo

optionsInfo :: Opt.ParserInfo Options
optionsInfo =
  Opt.info
    optionsParser
    mempty

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.strOption
          (  Opt.long "config"
          <> Opt.short 'c'
          <> Opt.value "./modulint.dhall"
          <> Opt.showDefault
          )
    <*> Opt.some (Opt.strArgument (Opt.metavar "<source path...>"))

