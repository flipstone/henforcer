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
  ( DocumentationChecker
  , PerModuleMaxUndocumented
  , perModuleMaxUndocumentedDecoder
  , PerModuleMinDocumented
  , perModuleMinDocumentedDecoder
  , checkDocs
  , newDocumentationChecker
  , docErrorMessagesFromList
  ) where


import qualified Data.Map.Strict as M
import qualified Dhall
import qualified Numeric.Natural as Nat
import qualified Pollock

import qualified CompatGHC
import qualified Henforcer.Config as Config
import qualified Henforcer.MaxUndocumented as Doc

data DocumentationChecker = DocumentationChecker
  { defaultMaxUndocumented :: Doc.DefaultAllowedUndocumentedExports
  , defaultMinDocumented :: Maybe Nat.Natural
  , perModuleMaxUndocumented :: Doc.PerModuleAllowedUndocumentedExports
  , perModuleMinDocumented :: PerModuleMinDocumented
  }

newDocumentationChecker :: Config.Config -> DocumentationChecker
newDocumentationChecker config =
  DocumentationChecker
    { defaultMaxUndocumented = Config.defaultMaxUndocumented config
    , defaultMinDocumented = Nothing
    , perModuleMaxUndocumented = Config.perModuleMaxUndocumented config
    , perModuleMinDocumented = PerModuleMinDocumented mempty
    }

checkDocs :: DocumentationChecker -> CompatGHC.ModuleName -> Pollock.ModuleInfo -> [CheckFailure]
checkDocs checker =
  checkUndocumented (defaultMaxUndocumented checker) (perModuleMaxUndocumented checker)

checkUndocumented :: Doc.DefaultAllowedUndocumentedExports
                  -> Doc.PerModuleAllowedUndocumentedExports
                  -> CompatGHC.ModuleName
                  -> Pollock.ModuleInfo
                  -> [CheckFailure]
checkUndocumented defMax perMod modName pollockModInfo =
  case Doc.determineMaxAllowedForModule defMax perMod modName of
    Doc.NoMaxToEnforce -> mempty
    Doc.MaxForModule maxForRule ->
      let
          undocumented = fromIntegral $ Pollock.haddockableExports pollockModInfo - Pollock.haddockedExports pollockModInfo
      in
        if  undocumented > maxForRule
        then
          pure (OverMaxUndocumented undocumented maxForRule)
        else
          mempty

newtype PerModuleMaxUndocumented
  = PerModuleMaxUndocumented (M.Map CompatGHC.ModuleName Nat.Natural)

newtype PerModuleMinDocumented
  = PerModuleMinDocumented (M.Map CompatGHC.ModuleName Nat.Natural)

perModuleMaxUndocumentedDecoder :: Dhall.Decoder PerModuleMaxUndocumented
perModuleMaxUndocumentedDecoder =
  fmap
    PerModuleMaxUndocumented
    (Dhall.map CompatGHC.moduleNameDecoder Dhall.natural)

perModuleMinDocumentedDecoder :: Dhall.Decoder PerModuleMinDocumented
perModuleMinDocumentedDecoder =
  fmap
    PerModuleMinDocumented
    (Dhall.map CompatGHC.moduleNameDecoder Dhall.natural)

instance CompatGHC.Outputable CheckFailure where
  ppr cf =
    case cf of
      OverMaxUndocumented current rule -> formatOverMaxUndocumentedViolation current rule
      UnderMinDocumented current rule -> formatUnderMinDocumentedViolation current rule

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
  = OverMaxUndocumented Doc.MaxUndocumentedExportsNat Doc.MaxUndocumentedExportsNat
  | UnderMinDocumented Nat.Natural Nat.Natural

formatUnderMinDocumentedViolation ::
  Nat.Natural -> Nat.Natural -> CompatGHC.SDoc
formatUnderMinDocumentedViolation current rule =
  let beginningDoc =
        ( CompatGHC.sep
              [ CompatGHC.hsep
                  [ CompatGHC.text "There were not enough documented exports. The min allowed is:"
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
  Doc.MaxUndocumentedExportsNat -> Doc.MaxUndocumentedExportsNat -> CompatGHC.SDoc
formatOverMaxUndocumentedViolation current rule =
  let beginningDoc =
        ( CompatGHC.sep
              [ CompatGHC.hsep
                  [ CompatGHC.text "There were too many undocumented exports. The max allowed is:"
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

instance CompatGHC.Outputable Nat.Natural where
  ppr n = CompatGHC.text $ show n

mkEnv :: CheckFailure -> CompatGHC.MsgEnvelope CheckFailure
mkEnv = CompatGHC.mkErrorMsgEnvelope CompatGHC.generatedSrcSpan CompatGHC.neverQualify

docErrorMessagesFromList :: [CheckFailure] -> CompatGHC.Messages CheckFailure
docErrorMessagesFromList =
  CompatGHC.mkMessagesFromList . fmap mkEnv
