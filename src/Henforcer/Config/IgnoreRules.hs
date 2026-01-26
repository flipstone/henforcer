{- |
Module      : Henforcer.Config.IgnoreRules
Description : Functionality for specifying that some rules in TOML configuration file  should be ignored.
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Config.IgnoreRules
  ( IgnoreRules (..)
  , RulesToIgnore (..)
  , rulestoIgnoreCodec
  , isRuleIgnored
  ) where

import Control.Applicative ((<|>))
import qualified Data.String as String
import qualified Toml

import qualified TomlHelper

{- | Do we want to ignore all rules or some that are listed. This serves as a way to make writing
the configuration for users easier, as they can quickly turn off all checks for a module.
-}
data RulesToIgnore
  = All !Bool
  | SpecificRulesToIgnore !IgnoreRules

matchAll :: RulesToIgnore -> Maybe Bool
matchAll (All b) = Just b
matchAll _ = Nothing

matchSpecificRulesToIgnore :: RulesToIgnore -> Maybe IgnoreRules
matchSpecificRulesToIgnore (All _) = Nothing
matchSpecificRulesToIgnore (SpecificRulesToIgnore rules) = Just rules

rulestoIgnoreCodec :: Toml.TomlCodec RulesToIgnore
rulestoIgnoreCodec =
  Toml.dimatch matchAll All (Toml.bool (String.fromString "all"))
    <|> Toml.dimatch matchSpecificRulesToIgnore SpecificRulesToIgnore ignoreRulesCodec

{- | Specify which rules to ignore, note that this is intended to be used when specifying modules in
some fashion. To ignore rules at the top level would be to simply not enable them.
-}
data IgnoreRules = IgnoreRules
  { ignoreRulesTreeDependencies :: !Bool
  , ignoreRulesEncapsulatedTrees :: !Bool
  , ignoreRulesAllowedQualifications :: !Bool
  , ignoreRulesAllowedOpenUnaliasedImports :: !Bool
  , ignoreRulesAllowedAliasUniqueness :: !Bool
  , ignoreRulesMaximumUndocumentedExports :: !Bool
  , ignoreRulesMinimumDocumentedExports :: !Bool
  , ignoreRulesMaximumExportsWithoutSince :: !Bool
  , ignoreRulesMinimumExportsWithSince :: !Bool
  , ignoreRulesModuleHeaderCopyrightMustExistNonEmpty :: !Bool
  , ignoreRulesModuleHeaderDescriptionMustExistNonEmpty :: !Bool
  , ignoreRulesModuleHeaderLicenseMustExistNonEmpty :: !Bool
  , ignoreRulesModuleHeaderMaintainerMustExistNonEmpty :: !Bool
  }

ignoreRulesCodec :: Toml.TomlCodec IgnoreRules
ignoreRulesCodec =
  IgnoreRules
    <$> TomlHelper.addField
      "treeDependencies"
      ignoreRulesTreeDependencies
      boolDefaultFalseField
    <*> TomlHelper.addField
      "encapsulatedTrees"
      ignoreRulesEncapsulatedTrees
      boolDefaultFalseField
    <*> TomlHelper.addField
      "allowedQualifications"
      ignoreRulesAllowedQualifications
      boolDefaultFalseField
    <*> TomlHelper.addField
      "allowedOpenUnaliasedImports"
      ignoreRulesAllowedOpenUnaliasedImports
      boolDefaultFalseField
    <*> TomlHelper.addField
      "allowedAliasUniqueness"
      ignoreRulesAllowedAliasUniqueness
      boolDefaultFalseField
    <*> TomlHelper.addField
      "maximumExportsPlusHeaderUndocumented"
      ignoreRulesMaximumUndocumentedExports
      boolDefaultFalseField
    <*> TomlHelper.addField
      "minimumExportsPlusHeaderDocumented"
      ignoreRulesMinimumDocumentedExports
      boolDefaultFalseField
    <*> TomlHelper.addField
      "maximumExportsWithoutSince"
      ignoreRulesMaximumExportsWithoutSince
      boolDefaultFalseField
    <*> TomlHelper.addField
      "minimumExportsWithSince"
      ignoreRulesMinimumExportsWithSince
      boolDefaultFalseField
    <*> TomlHelper.addField
      "moduleHeaderCopyrightMustExistNonEmpty"
      ignoreRulesModuleHeaderCopyrightMustExistNonEmpty
      boolDefaultFalseField
    <*> TomlHelper.addField
      "moduleHeaderDescriptionMustExistNonEmpty"
      ignoreRulesModuleHeaderDescriptionMustExistNonEmpty
      boolDefaultFalseField
    <*> TomlHelper.addField
      "moduleHeaderLicenseMustExistNonEmpty"
      ignoreRulesModuleHeaderLicenseMustExistNonEmpty
      boolDefaultFalseField
    <*> TomlHelper.addField
      "moduleHeaderMaintainerMustExistNonEmpty"
      ignoreRulesModuleHeaderMaintainerMustExistNonEmpty
      boolDefaultFalseField

-- | Default a 'Bool' field
boolDefaultFalseField :: Toml.Key -> Toml.TomlCodec Bool
boolDefaultFalseField =
  TomlHelper.setDefault False Toml.bool

isRuleIgnored ::
  (IgnoreRules -> Bool)
  -> RulesToIgnore
  -> Bool
isRuleIgnored _ (All b) = b
isRuleIgnored accessor (SpecificRulesToIgnore ignoreRules) =
  accessor ignoreRules
