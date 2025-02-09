{- |
Module      : Henforcer.Options
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2025
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Options
  ( Options (..)
  , parseGivenOptions
  ) where

import qualified Options.Applicative as Opt

newtype Options = Options
  { configPath :: FilePath
  }

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
