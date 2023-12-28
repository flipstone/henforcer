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
      OverMaxUndocumented current rule -> formatOverMaxUndocumentedViolation current rule
      UnderMinDocumented current rule -> formatUnderMinDocumentedViolation current rule
      OverMaximumWithoutSince current rule -> formatOverMaximumWithoutSinceViolation current rule
      UnderMinimumWithSince current rule -> formatUnderMinimumWithSinceViolation current rule

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
  = OverMaxUndocumented Rules.MaximumNat Rules.MaximumNat
  | UnderMinDocumented Rules.MinimumNat Rules.MinimumNat
  | OverMaximumWithoutSince Rules.MaximumNat Rules.MaximumNat
  | UnderMinimumWithSince Rules.MinimumNat Rules.MinimumNat

formatUnderMinDocumentedViolation ::
  Rules.MinimumNat -> Rules.MinimumNat -> CompatGHC.SDoc
formatUnderMinDocumentedViolation current rule =
  let beginningDoc =
        ( CompatGHC.sep
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
        )
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

formatOverMaxUndocumentedViolation ::
  Rules.MaximumNat -> Rules.MaximumNat -> CompatGHC.SDoc
formatOverMaxUndocumentedViolation current rule =
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
  }

determineDocumentationChecks ::
  Config.Config
  -> CompatGHC.ModuleName
  -> DocumentationChecks
determineDocumentationChecks config modName =
  let
    forSpecifiedModule =
      maybe Config.emptyForSpecifiedModule id
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
   in
    undocumentedChecks <> documentedChecks <> withoutSinceChecks <> withSinceChecks

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
      (OverMaxUndocumented . getUndocumented)

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
      (UnderMinDocumented . getDocumentedNat)

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
