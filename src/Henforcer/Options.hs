{- |
Module      : Henforcer.Options
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Options
  ( Options (..)
  , parseOptions
  , parseGivenOptions
  ) where

import qualified Options.Applicative as Opt

data Options = Options
  { configPath :: FilePath
  , initialize :: Bool
  }

parseOptions :: IO Options
parseOptions =
  Opt.execParser optionsInfo

parseGivenOptions :: [String] -> IO Options
parseGivenOptions =
  Opt.handleParseResult . Opt.execParserPure Opt.defaultPrefs optionsInfo

optionsInfo :: Opt.ParserInfo Options
optionsInfo =
  Opt.info
    optionsParser
    mempty

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.strOption
      ( Opt.long "config"
          <> Opt.short 'c'
          <> Opt.value "./henforcer.toml"
          <> Opt.showDefault
      )
    <*> Opt.flag False True (Opt.long "init")
