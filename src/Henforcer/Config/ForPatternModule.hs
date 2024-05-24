{- |
Module      : Henforcer.Config.ForPatternModule
Description : Functionality for specifying rules scoped to a particular module.
Copyright   : (c) Flipstone Technology Partners, 2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config.ForPatternModule
  ( moduleMatchesPattern
  , ForPatternModule
  , ForPatternModules
  , forPatternModulesCodecField
  , unionPatternAndSpecifiedModule
  ) where

import qualified Control.Applicative as A
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified System.FilePattern as FP
import qualified Toml

import qualified Henforcer.Config.ForSpecifiedModule as FSM

moduleMatchesPattern :: String -> ForPatternModules -> Maybe ForPatternModule
moduleMatchesPattern moduleName =
  List.find (wildcardMatch moduleName . fst)

wildcardMatch :: String -> FP.FilePattern -> Bool
wildcardMatch str pat =
  Maybe.isJust $
    FP.match
      (replaceSeparator pat)
      (replaceSeparator str)

replaceSeparator :: String -> String
replaceSeparator [] = []
replaceSeparator str@(x : xs) =
  case List.stripPrefix "." str of
    Just stripped -> "/" <> replaceSeparator stripped
    Nothing -> x : replaceSeparator xs

type ForPatternModule = (FP.FilePattern, FSM.ForSpecifiedModule)
type ForPatternModules = [ForPatternModule]

forPatternModulesCodecField :: Toml.Key -> Toml.TomlCodec ForPatternModules
forPatternModulesCodecField =
  Toml.list (Toml.pair (Toml.string $ String.fromString "pattern") FSM.forSpecifiedModuleCodec)

unionPatternAndSpecifiedModule ::
  FSM.ForSpecifiedModule -> FSM.ForSpecifiedModule -> FSM.ForSpecifiedModule
unionPatternAndSpecifiedModule fpm fsm =
  FSM.ForSpecifiedModule
    { FSM.specifiedModuleAllowedQualifications =
        FSM.specifiedModuleAllowedQualifications fsm
          A.<|> FSM.specifiedModuleAllowedQualifications fpm
    , FSM.specifiedModuleAllowedOpenUnaliasedImports =
        FSM.specifiedModuleAllowedOpenUnaliasedImports fsm
          A.<|> FSM.specifiedModuleAllowedOpenUnaliasedImports fpm
    , FSM.specifiedModuleAllowedAliasUniqueness =
        FSM.specifiedModuleAllowedAliasUniqueness fsm
          A.<|> FSM.specifiedModuleAllowedAliasUniqueness fpm
    , FSM.specifiedModuleMaximumUndocumentedExports =
        FSM.specifiedModuleMaximumUndocumentedExports fsm
          A.<|> FSM.specifiedModuleMaximumUndocumentedExports fpm
    , FSM.specifiedModuleMinimumDocumentedExports =
        FSM.specifiedModuleMinimumDocumentedExports fsm
          A.<|> FSM.specifiedModuleMinimumDocumentedExports fpm
    , FSM.specifiedModuleMaximumExportsWithoutSince =
        FSM.specifiedModuleMaximumExportsWithoutSince fsm
          A.<|> FSM.specifiedModuleMaximumExportsWithoutSince fpm
    , FSM.specifiedModuleMinimumExportsWithSince =
        FSM.specifiedModuleMinimumExportsWithSince fsm
          A.<|> FSM.specifiedModuleMinimumExportsWithSince fpm
    , FSM.specifiedModuleModuleHeaderCopyrightMustExistNonEmpty =
        FSM.specifiedModuleModuleHeaderCopyrightMustExistNonEmpty fsm
          A.<|> FSM.specifiedModuleModuleHeaderCopyrightMustExistNonEmpty fpm
    , FSM.specifiedModuleModuleHeaderDescriptionMustExistNonEmpty =
        FSM.specifiedModuleModuleHeaderDescriptionMustExistNonEmpty fsm
          A.<|> FSM.specifiedModuleModuleHeaderDescriptionMustExistNonEmpty fpm
    , FSM.specifiedModuleModuleHeaderLicenseMustExistNonEmpty =
        FSM.specifiedModuleModuleHeaderLicenseMustExistNonEmpty fsm
          A.<|> FSM.specifiedModuleModuleHeaderLicenseMustExistNonEmpty fpm
    , FSM.specifiedModuleModuleHeaderMaintainerMustExistNonEmpty =
        FSM.specifiedModuleModuleHeaderMaintainerMustExistNonEmpty fsm
          A.<|> FSM.specifiedModuleModuleHeaderMaintainerMustExistNonEmpty fpm
    , FSM.specifiedModuleRulesToIgnore =
        FSM.specifiedModuleRulesToIgnore fsm
          A.<|> FSM.specifiedModuleRulesToIgnore fpm
    }
