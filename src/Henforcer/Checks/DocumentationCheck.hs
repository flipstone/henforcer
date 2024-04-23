{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Henforcer.Checks.DocumentationCheck
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Checks.DocumentationCheck
  ( docErrorMessagesFromList
  , determineDocumentationChecks
  , checkDocumentation
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Numeric.Natural as Nat
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
  = OverMaximumUndocumented Rules.MaximumNat Rules.MaximumNat
  | UnderMinimumDocumented Rules.MinimumNat Rules.MinimumNat
  | OverMaximumWithoutSince Rules.MaximumNat Rules.MaximumNat
  | UnderMinimumWithSince Rules.MinimumNat Rules.MinimumNat
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
        ( CompatGHC.sep
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
        )
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

formatOverMaximumWithoutSinceViolation ::
  Rules.MaximumNat -> Rules.MaximumNat -> CompatGHC.SDoc
formatOverMaximumWithoutSinceViolation current rule =
  let beginningDoc =
        ( CompatGHC.sep
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
        )
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

formatUnderMinimumWithSinceViolation ::
  Rules.MinimumNat -> Rules.MinimumNat -> CompatGHC.SDoc
formatUnderMinimumWithSinceViolation current rule =
  let beginningDoc =
        ( CompatGHC.sep
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
        )
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

instance CompatGHC.Outputable Nat.Natural where
  ppr n = CompatGHC.text $ show n

mkEnv :: CheckFailure -> CompatGHC.MsgEnvelope CheckFailure
mkEnv = CompatGHC.mkErrorMsgEnvelope CompatGHC.generatedSrcSpan CompatGHC.neverQualify

docErrorMessagesFromList :: [CheckFailure] -> CompatGHC.Messages CheckFailure
docErrorMessagesFromList =
  CompatGHC.mkMessagesFromList . fmap mkEnv

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

determineDocumentationChecks ::
  Config.Config
  -> CompatGHC.ModuleName
  -> DocumentationChecks
determineDocumentationChecks config modName =
  let
    forSpecifiedModule =
      Maybe.fromMaybe Config.emptyForSpecifiedModule
        . Map.lookup modName
        $ Config.forSpecifiedModules config
    forAnyModule = Config.forAnyModule config
   in
    DocumentationChecks
      { maximumUndocumentedExports =
          Maybe.fromMaybe
            (Config.anyModuleMaximumUndocumentedExports forAnyModule)
            (Config.specifiedModuleMaximumUndocumentedExports forSpecifiedModule)
      , minimumDocumentedExports =
          Maybe.fromMaybe
            (Config.anyModuleMinimumDocumentedExports forAnyModule)
            (Config.specifiedModuleMinimumDocumentedExports forSpecifiedModule)
      , maximumExportsWithoutSince =
          Maybe.fromMaybe
            (Config.anyModuleMaximumExportsWithoutSince forAnyModule)
            (Config.specifiedModuleMaximumExportsWithoutSince forSpecifiedModule)
      , minimumExportsWithSince =
          Maybe.fromMaybe
            (Config.anyModuleMinimumExportsWithSince forAnyModule)
            (Config.specifiedModuleMinimumExportsWithSince forSpecifiedModule)
      , moduleHeaderCopyrightMustExistNonEmpty =
          Maybe.fromMaybe
            (Config.anyModuleModuleHeaderCopyrightMustExistNonEmpty forAnyModule)
            (Config.specifiedModuleModuleHeaderCopyrightMustExistNonEmpty forSpecifiedModule)
      , moduleHeaderDescriptionMustExistNonEmpty =
          Maybe.fromMaybe
            (Config.anyModuleModuleHeaderDescriptionMustExistNonEmpty forAnyModule)
            (Config.specifiedModuleModuleHeaderDescriptionMustExistNonEmpty forSpecifiedModule)
      , moduleHeaderLicenseMustExistNonEmpty =
          Maybe.fromMaybe
            (Config.anyModuleModuleHeaderLicenseMustExistNonEmpty forAnyModule)
            (Config.specifiedModuleModuleHeaderLicenseMustExistNonEmpty forSpecifiedModule)
      , moduleHeaderMaintainerMustExistNonEmpty =
          Maybe.fromMaybe
            (Config.anyModuleModuleHeaderMaintainerMustExistNonEmpty forAnyModule)
            (Config.specifiedModuleModuleHeaderMaintainerMustExistNonEmpty forSpecifiedModule)
      }

checkDocumentation ::
  DocumentationChecks
  -> Pollock.ModuleInfo
  -> [CheckFailure]
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
  Rules.MaximumAllowed
  -> Pollock.ModuleInfo
  -> [CheckFailure]
checkUndocumented maximumAllowed pollockModInfo =
  let
    getUndocumented x = fromIntegral $ Pollock.haddockableExports x - Pollock.haddockedExports x
   in
    Rules.checkMaximum
      maximumAllowed
      pollockModInfo
      getUndocumented
      (OverMaximumUndocumented . getUndocumented)

checkDocumented ::
  Rules.MinimumAllowed
  -> Pollock.ModuleInfo
  -> [CheckFailure]
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
  Rules.MaximumAllowed
  -> Pollock.ModuleInfo
  -> [CheckFailure]
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
  Rules.MinimumAllowed
  -> Pollock.ModuleInfo
  -> [CheckFailure]
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
  Rules.MustExistNonEmpty
  -> Pollock.ModuleInfo
  -> [CheckFailure]
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
  Rules.MustExistNonEmpty
  -> Pollock.ModuleInfo
  -> [CheckFailure]
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
  Rules.MustExistNonEmpty
  -> Pollock.ModuleInfo
  -> [CheckFailure]
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
  Rules.MustExistNonEmpty
  -> Pollock.ModuleInfo
  -> [CheckFailure]
checkModuleHeaderMaintainer maintainerMustExist pollockModInfo =
  let
    getMaintainer = Pollock.maintainer . Pollock.moduleHeader
   in
    Rules.checkExistsAndNonEmptyString
      maintainerMustExist
      pollockModInfo
      getMaintainer
      (const MaintainerMustBeNonEmpty)
