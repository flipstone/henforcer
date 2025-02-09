{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

{- |
Module      : Henforcer.Checks.DocumentationCheck
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2025
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Checks.DocumentationCheck
  ( docErrorMessagesFromList
  , determineDocumentationChecks
  , checkDocumentation
  , DocumentationChecks (..)
  ) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Pollock

import qualified CompatGHC
import qualified Henforcer.Config as Config
import qualified Henforcer.Rules as Rules

instance CompatGHC.Outputable CheckFailure where
  ppr cf =
    case cf of
      OverMaximumUndocumented current rule -> formatOverMaximumUndocumentedViolation current rule
      UnderMinimumDocumented current rule -> formatUnderMinimumDocumentedViolation current rule
      OverMaximumWithoutSince current rule -> formatOverMaximumWithoutSinceViolation current rule
      UnderMinimumWithSince current rule -> formatUnderMinimumWithSinceViolation current rule
      CopyrightMustBeNonEmpty ->
        formatMustBeNonEmpty "copyright"
      DescriptionMustBeNonEmpty ->
        formatMustBeNonEmpty "description"
      LicenseMustBeNonEmpty ->
        formatMustBeNonEmpty "license"
      MaintainerMustBeNonEmpty ->
        formatMustBeNonEmpty "maintainer"

{- | The only part of the 'CompatGHC.Diagnostic' class that we really care about is the
 'diagnosticMessage', used for printing.
-}
instance CompatGHC.Diagnostic CheckFailure where
  type DiagnosticOpts CheckFailure = CompatGHC.NoDiagnosticOpts
  diagnosticMessage _ = CompatGHC.mkSimpleDecorated . CompatGHC.ppr
  diagnosticReason = const CompatGHC.ErrorWithoutFlag
  diagnosticHints = const mempty
  diagnosticCode = const Nothing

data CheckFailure
  = OverMaximumUndocumented !Rules.MaximumNat !Rules.MaximumNat
  | UnderMinimumDocumented !Rules.MinimumNat !Rules.MinimumNat
  | OverMaximumWithoutSince !Rules.MaximumNat !Rules.MaximumNat
  | UnderMinimumWithSince !Rules.MinimumNat !Rules.MinimumNat
  | CopyrightMustBeNonEmpty
  | DescriptionMustBeNonEmpty
  | LicenseMustBeNonEmpty
  | MaintainerMustBeNonEmpty

formatUnderMinimumDocumentedViolation ::
  Rules.MinimumNat -> Rules.MinimumNat -> CompatGHC.SDoc
formatUnderMinimumDocumentedViolation current rule =
  let beginningDoc =
        CompatGHC.sep
          [ CompatGHC.hsep
              [ CompatGHC.text "There were not enough documented exports."
              ]
          , CompatGHC.hsep
              [ CompatGHC.text "The minimum allowed is:"
              , CompatGHC.ppr rule
              ]
          , CompatGHC.hsep
              [ CompatGHC.text "The number documented is:"
              , CompatGHC.ppr current
              ]
          ]
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

formatOverMaximumUndocumentedViolation ::
  Rules.MaximumNat -> Rules.MaximumNat -> CompatGHC.SDoc
formatOverMaximumUndocumentedViolation current rule =
  let beginningDoc =
        CompatGHC.sep
          [ CompatGHC.hsep
              [ CompatGHC.text "There were too many undocumented exports."
              ]
          , CompatGHC.hsep
              [ CompatGHC.text "The maximum allowed is:"
              , CompatGHC.ppr rule
              ]
          , CompatGHC.hsep
              [ CompatGHC.text "The number undocumented is:"
              , CompatGHC.ppr current
              ]
          ]
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

formatOverMaximumWithoutSinceViolation ::
  Rules.MaximumNat -> Rules.MaximumNat -> CompatGHC.SDoc
formatOverMaximumWithoutSinceViolation current rule =
  let beginningDoc =
        CompatGHC.sep
          [ CompatGHC.hsep
              [ CompatGHC.text "There were too many exports without an @since haddock annotation."
              ]
          , CompatGHC.hsep
              [ CompatGHC.text "The maximum allowed is:"
              , CompatGHC.ppr rule
              ]
          , CompatGHC.hsep
              [ CompatGHC.text "The number missing the annotation is:"
              , CompatGHC.ppr current
              ]
          ]
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

formatUnderMinimumWithSinceViolation ::
  Rules.MinimumNat -> Rules.MinimumNat -> CompatGHC.SDoc
formatUnderMinimumWithSinceViolation current rule =
  let beginningDoc =
        CompatGHC.sep
          [ CompatGHC.hsep
              [ CompatGHC.text "There were not enough exports with an @since haddock annotation."
              ]
          , CompatGHC.hsep
              [ CompatGHC.text "The minimum allowed is:"
              , CompatGHC.ppr rule
              ]
          , CompatGHC.hsep
              [ CompatGHC.text "The number with the annotation is:"
              , CompatGHC.ppr current
              ]
          ]
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

formatMustBeNonEmpty ::
  String
  -> CompatGHC.SDoc
formatMustBeNonEmpty fieldStr =
  CompatGHC.vcat
    [ CompatGHC.sep
        [ CompatGHC.hsep
            [ CompatGHC.text ("The module header field " <> fieldStr <> " must be present and non-empty.")
            ]
        ]
    , CompatGHC.blankLine
    ]

mkEnv :: CheckFailure -> CompatGHC.MsgEnvelope CheckFailure
mkEnv = CompatGHC.mkErrorMsgEnvelope CompatGHC.generatedSrcSpan CompatGHC.neverQualify

docErrorMessagesFromList :: CompatGHC.Bag CheckFailure -> CompatGHC.Messages CheckFailure
docErrorMessagesFromList =
  CompatGHC.mkMessages . fmap mkEnv

data DocumentationChecks = DocumentationChecks
  { maximumUndocumentedExports :: !Rules.MaximumAllowed
  , minimumDocumentedExports :: !Rules.MinimumAllowed
  , maximumExportsWithoutSince :: !Rules.MaximumAllowed
  , minimumExportsWithSince :: !Rules.MinimumAllowed
  , moduleHeaderCopyrightMustExistNonEmpty :: !Rules.MustExistNonEmpty
  , moduleHeaderDescriptionMustExistNonEmpty :: !Rules.MustExistNonEmpty
  , moduleHeaderLicenseMustExistNonEmpty :: !Rules.MustExistNonEmpty
  , moduleHeaderMaintainerMustExistNonEmpty :: !Rules.MustExistNonEmpty
  }
  deriving (Eq, Show)

determineDocumentationChecks ::
  Config.Config
  -> CompatGHC.ModuleName
  -> DocumentationChecks
determineDocumentationChecks config modName =
  let
    originalSpecifiedModule =
      Maybe.fromMaybe Config.emptyForSpecifiedModule
        . List.lookup modName
        $ Config.forSpecifiedModules config
    forAnyModule = Config.forAnyModule config
    patternModule =
      maybe Config.emptyForSpecifiedModule snd
        . Config.moduleMatchesPattern (CompatGHC.moduleNameString modName)
        $ Config.forPatternModules config
    forSpecifiedModule = Config.unionPatternAndSpecifiedModule patternModule originalSpecifiedModule
   in
    DocumentationChecks
      { maximumUndocumentedExports =
          determineMaxiumumUndocumentedExports forAnyModule forSpecifiedModule
      , minimumDocumentedExports =
          determineMiniumumDocumentedExports forAnyModule forSpecifiedModule
      , maximumExportsWithoutSince =
          determineMaxiumumExportsWithoutSince forAnyModule forSpecifiedModule
      , minimumExportsWithSince =
          determineMiniumumExportsWithSince forAnyModule forSpecifiedModule
      , moduleHeaderCopyrightMustExistNonEmpty =
          determineModuleHeaderCopyrightMustExistNonEmpty forAnyModule forSpecifiedModule
      , moduleHeaderDescriptionMustExistNonEmpty =
          determineModuleHeaderDescriptionMustExistNonEmpty forAnyModule forSpecifiedModule
      , moduleHeaderLicenseMustExistNonEmpty =
          determineModuleHeaderLicenseMustExistNonEmpty forAnyModule forSpecifiedModule
      , moduleHeaderMaintainerMustExistNonEmpty =
          determineModuleHeaderMaintainerMustExistNonEmpty forAnyModule forSpecifiedModule
      }

determineMaxiumumUndocumentedExports ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> Rules.MaximumAllowed
determineMaxiumumUndocumentedExports forAnyModule forSpecifiedModule =
  let
    shouldBeIgnored =
      maybe False (Config.isRuleIgnored Config.ignoreRulesMaximumUndocumentedExports) $
        Config.specifiedModuleRulesToIgnore forSpecifiedModule
   in
    if shouldBeIgnored
      then Rules.NotEnforced
      else
        Maybe.fromMaybe
          (Config.anyModuleMaximumUndocumentedExports forAnyModule)
          (Config.specifiedModuleMaximumUndocumentedExports forSpecifiedModule)

determineMiniumumDocumentedExports ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> Rules.MinimumAllowed
determineMiniumumDocumentedExports forAnyModule forSpecifiedModule =
  let
    shouldBeIgnored =
      maybe False (Config.isRuleIgnored Config.ignoreRulesMinimumDocumentedExports) $
        Config.specifiedModuleRulesToIgnore forSpecifiedModule
   in
    if shouldBeIgnored
      then Rules.NotEnforced
      else
        Maybe.fromMaybe
          (Config.anyModuleMinimumDocumentedExports forAnyModule)
          (Config.specifiedModuleMinimumDocumentedExports forSpecifiedModule)

determineMaxiumumExportsWithoutSince ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> Rules.MaximumAllowed
determineMaxiumumExportsWithoutSince forAnyModule forSpecifiedModule =
  let
    shouldBeIgnored =
      maybe False (Config.isRuleIgnored Config.ignoreRulesMaximumExportsWithoutSince) $
        Config.specifiedModuleRulesToIgnore forSpecifiedModule
   in
    if shouldBeIgnored
      then Rules.NotEnforced
      else
        Maybe.fromMaybe
          (Config.anyModuleMaximumExportsWithoutSince forAnyModule)
          (Config.specifiedModuleMaximumExportsWithoutSince forSpecifiedModule)

determineMiniumumExportsWithSince ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> Rules.MinimumAllowed
determineMiniumumExportsWithSince forAnyModule forSpecifiedModule =
  let
    shouldBeIgnored =
      maybe False (Config.isRuleIgnored Config.ignoreRulesMinimumExportsWithSince) $
        Config.specifiedModuleRulesToIgnore forSpecifiedModule
   in
    if shouldBeIgnored
      then Rules.NotEnforced
      else
        Maybe.fromMaybe
          (Config.anyModuleMinimumExportsWithSince forAnyModule)
          (Config.specifiedModuleMinimumExportsWithSince forSpecifiedModule)

determineModuleHeaderCopyrightMustExistNonEmpty ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> Rules.MustExistNonEmpty
determineModuleHeaderCopyrightMustExistNonEmpty forAnyModule forSpecifiedModule =
  let
    shouldBeIgnored =
      maybe False (Config.isRuleIgnored Config.ignoreRulesModuleHeaderCopyrightMustExistNonEmpty) $
        Config.specifiedModuleRulesToIgnore forSpecifiedModule
   in
    if shouldBeIgnored
      then Rules.NotEnforced
      else
        Maybe.fromMaybe
          (Config.anyModuleModuleHeaderCopyrightMustExistNonEmpty forAnyModule)
          (Config.specifiedModuleModuleHeaderCopyrightMustExistNonEmpty forSpecifiedModule)

determineModuleHeaderDescriptionMustExistNonEmpty ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> Rules.MustExistNonEmpty
determineModuleHeaderDescriptionMustExistNonEmpty forAnyModule forSpecifiedModule =
  let
    shouldBeIgnored =
      maybe False (Config.isRuleIgnored Config.ignoreRulesModuleHeaderDescriptionMustExistNonEmpty) $
        Config.specifiedModuleRulesToIgnore forSpecifiedModule
   in
    if shouldBeIgnored
      then Rules.NotEnforced
      else
        Maybe.fromMaybe
          (Config.anyModuleModuleHeaderDescriptionMustExistNonEmpty forAnyModule)
          (Config.specifiedModuleModuleHeaderDescriptionMustExistNonEmpty forSpecifiedModule)

determineModuleHeaderLicenseMustExistNonEmpty ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> Rules.MustExistNonEmpty
determineModuleHeaderLicenseMustExistNonEmpty forAnyModule forSpecifiedModule =
  let
    shouldBeIgnored =
      maybe False (Config.isRuleIgnored Config.ignoreRulesModuleHeaderLicenseMustExistNonEmpty) $
        Config.specifiedModuleRulesToIgnore forSpecifiedModule
   in
    if shouldBeIgnored
      then Rules.NotEnforced
      else
        Maybe.fromMaybe
          (Config.anyModuleModuleHeaderLicenseMustExistNonEmpty forAnyModule)
          (Config.specifiedModuleModuleHeaderLicenseMustExistNonEmpty forSpecifiedModule)

determineModuleHeaderMaintainerMustExistNonEmpty ::
  Config.ForAnyModule
  -> Config.ForSpecifiedModule
  -> Rules.MustExistNonEmpty
determineModuleHeaderMaintainerMustExistNonEmpty forAnyModule forSpecifiedModule =
  let
    shouldBeIgnored =
      maybe False (Config.isRuleIgnored Config.ignoreRulesModuleHeaderMaintainerMustExistNonEmpty) $
        Config.specifiedModuleRulesToIgnore forSpecifiedModule
   in
    if shouldBeIgnored
      then Rules.NotEnforced
      else
        Maybe.fromMaybe
          (Config.anyModuleModuleHeaderMaintainerMustExistNonEmpty forAnyModule)
          (Config.specifiedModuleModuleHeaderMaintainerMustExistNonEmpty forSpecifiedModule)

checkDocumentation ::
  (Applicative m, Monoid (m CheckFailure)) =>
  DocumentationChecks
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkDocumentation checks pollockModInfo =
  let
    undocumentedChecks =
      checkUndocumented
        (maximumUndocumentedExports checks)
        pollockModInfo
    documentedChecks =
      checkDocumented
        (minimumDocumentedExports checks)
        pollockModInfo
    withoutSinceChecks =
      checkWithoutSince
        (maximumExportsWithoutSince checks)
        pollockModInfo
    withSinceChecks =
      checkWithSince
        (minimumExportsWithSince checks)
        pollockModInfo
    withCopyrightChecks =
      checkModuleHeaderCopyright
        (moduleHeaderCopyrightMustExistNonEmpty checks)
        pollockModInfo
    withDescriptionChecks =
      checkModuleHeaderDescription
        (moduleHeaderDescriptionMustExistNonEmpty checks)
        pollockModInfo
    withLicenseChecks =
      checkModuleHeaderLicense
        (moduleHeaderLicenseMustExistNonEmpty checks)
        pollockModInfo
    withMaintainerChecks =
      checkModuleHeaderMaintainer
        (moduleHeaderMaintainerMustExistNonEmpty checks)
        pollockModInfo
   in
    undocumentedChecks
      <> documentedChecks
      <> withoutSinceChecks
      <> withSinceChecks
      <> withCopyrightChecks
      <> withDescriptionChecks
      <> withLicenseChecks
      <> withMaintainerChecks

checkUndocumented ::
  (Applicative m, Monoid (m CheckFailure)) =>
  Rules.MaximumAllowed
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkUndocumented maximumAllowed pollockModInfo =
  let
    getUndocumented :: Pollock.ModuleInfo -> Rules.MaximumNat
    getUndocumented x = fromIntegral $ Pollock.haddockableExports x - Pollock.haddockedExports x
   in
    Rules.checkMaximum
      maximumAllowed
      pollockModInfo
      getUndocumented
      (OverMaximumUndocumented . getUndocumented)

checkDocumented ::
  (Applicative m, Monoid (m CheckFailure)) =>
  Rules.MinimumAllowed
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkDocumented minimumAllowed pollockModInfo =
  let
    getDocumentedNat :: Pollock.ModuleInfo -> Rules.MinimumNat
    getDocumentedNat = fromIntegral . Pollock.haddockedExports
   in
    Rules.checkMinimum
      minimumAllowed
      pollockModInfo
      getDocumentedNat
      (UnderMinimumDocumented . getDocumentedNat)

checkWithoutSince ::
  (Applicative m, Monoid (m CheckFailure)) =>
  Rules.MaximumAllowed
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkWithoutSince maximumAllowed pollockModInfo =
  let
    numWithoutSinceNat :: Pollock.ModuleInfo -> Rules.MaximumNat
    numWithoutSinceNat x = fromIntegral $ Pollock.haddockableExports x - Pollock.numWithSince x
   in
    Rules.checkMaximum
      maximumAllowed
      pollockModInfo
      numWithoutSinceNat
      (OverMaximumWithoutSince . numWithoutSinceNat)

checkWithSince ::
  (Applicative m, Monoid (m CheckFailure)) =>
  Rules.MinimumAllowed
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkWithSince minimumAllowed pollockModInfo =
  let
    numWithSinceNat :: Pollock.ModuleInfo -> Rules.MinimumNat
    numWithSinceNat = fromIntegral . Pollock.numWithSince
   in
    Rules.checkMinimum
      minimumAllowed
      pollockModInfo
      numWithSinceNat
      (UnderMinimumWithSince . numWithSinceNat)

checkModuleHeaderCopyright ::
  (Applicative m, Monoid (m CheckFailure)) =>
  Rules.MustExistNonEmpty
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkModuleHeaderCopyright copyrightMustExist pollockModInfo =
  let
    getCopyright = Pollock.copyright . Pollock.moduleHeader
   in
    Rules.checkExistsAndNonEmptyString
      copyrightMustExist
      pollockModInfo
      getCopyright
      (const CopyrightMustBeNonEmpty)

checkModuleHeaderDescription ::
  (Applicative m, Monoid (m CheckFailure)) =>
  Rules.MustExistNonEmpty
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkModuleHeaderDescription descriptionMustExist pollockModInfo =
  let
    getDescription = Pollock.description . Pollock.moduleHeader
   in
    Rules.checkExistsAndNonEmptyString
      descriptionMustExist
      pollockModInfo
      getDescription
      (const DescriptionMustBeNonEmpty)

checkModuleHeaderLicense ::
  (Applicative m, Monoid (m CheckFailure)) =>
  Rules.MustExistNonEmpty
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkModuleHeaderLicense licenseMustExist pollockModInfo =
  let
    getLicense = Pollock.license . Pollock.moduleHeader
   in
    Rules.checkExistsAndNonEmptyString
      licenseMustExist
      pollockModInfo
      getLicense
      (const LicenseMustBeNonEmpty)

checkModuleHeaderMaintainer ::
  (Applicative m, Monoid (m CheckFailure)) =>
  Rules.MustExistNonEmpty
  -> Pollock.ModuleInfo
  -> m CheckFailure
checkModuleHeaderMaintainer maintainerMustExist pollockModInfo =
  let
    getMaintainer = Pollock.maintainer . Pollock.moduleHeader
   in
    Rules.checkExistsAndNonEmptyString
      maintainerMustExist
      pollockModInfo
      getMaintainer
      (const MaintainerMustBeNonEmpty)
