{- |
Module      : Henforcer.Config.Config
Description : Haskell representation of the toml configuration file that is used to define the checks to be enforced. As well as related functionality.
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config.Config
  ( Config (..)
  , loadConfig
  ) where

import qualified Toml

import qualified TomlHelper

import Henforcer.Config.ForAnyModule (ForAnyModule, emptyForAnyModule, forAnyModuleCodec)
import Henforcer.Config.ForPatternModule (ForPatternModules, forPatternModulesCodecField)
import Henforcer.Config.ForSpecifiedModule (ForSpecifiedModules, forSpecifiedModulesCodecField)

data Config = Config
  { forAnyModule :: !ForAnyModule
  , forPatternModules :: !ForPatternModules
  , forSpecifiedModules :: !ForSpecifiedModules
  }

configCodec :: Toml.TomlCodec Config
configCodec =
  Config
    <$> TomlHelper.addField
      "forAnyModule"
      forAnyModule
      (TomlHelper.setDefault emptyForAnyModule (Toml.table forAnyModuleCodec))
    <*> TomlHelper.addField
      "forPatternModules"
      forPatternModules
      forPatternModulesCodecField
    <*> TomlHelper.addField
      "forSpecifiedModules"
      forSpecifiedModules
      forSpecifiedModulesCodecField

loadConfig :: FilePath -> IO Config
loadConfig =
  Toml.decodeFile configCodec
