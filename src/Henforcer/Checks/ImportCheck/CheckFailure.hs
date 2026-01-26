{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

{- |
Module      : Henforcer.Checks.ImportCheck.CheckFailure
Description :
Copyright   : (c) Flipstone Technology Partners, 2023-2026
License     : MIT
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Checks.ImportCheck.CheckFailure
  ( CheckFailure (..)
  , CheckFailureWithNote
  , CheckedDependency (..)
  , errorMessagesFromList
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T

import qualified CompatGHC
import qualified Henforcer.CodeStructure as CodeStructure
import qualified Henforcer.Rules as Rules

data CheckedDependency = CheckedDependency
  { dependencySource :: !CodeStructure.ModuleTree
  , dependencyNote :: !Rules.UserNote
  , dependencyTarget :: !CodeStructure.ModuleTree
  }
  deriving (Eq, Show)

type CheckFailureWithNote = Rules.FailureWithUserNote CheckFailure

data CheckFailure
  = DependencyViolation !CodeStructure.Import !CheckedDependency
  | EncapsulationViolation !CodeStructure.Import !CodeStructure.ModuleTree
  | QualificationViolation !CodeStructure.Import ![CodeStructure.Scheme]
  | OpenImportViolation !Rules.MaximumNat !(NEL.NonEmpty CodeStructure.Import)
  | AliasUniquenessViolation !(NEL.NonEmpty CodeStructure.Import)

instance CompatGHC.Outputable CheckFailure where
  ppr cf =
    case cf of
      DependencyViolation i cd -> formatDependencyViolation i cd
      EncapsulationViolation i tn -> formatEncapsulationViolation i tn
      QualificationViolation i s -> formatQualificationViolation i s
      OpenImportViolation n i -> formatOpenImportViolation i n
      AliasUniquenessViolation is -> formatAliasUniquenssViolation is

checkFailureDiagnosticCode :: CheckFailure -> CompatGHC.DiagnosticCode
checkFailureDiagnosticCode cf =
  CompatGHC.mkHenforcerDiagnosticCode $
    case cf of
      DependencyViolation _ _ -> 49096
      EncapsulationViolation _ _ -> 53012
      QualificationViolation _ _ -> 12259
      OpenImportViolation _ _ -> 75306
      AliasUniquenessViolation _ -> 42555

{- | Convert a list of 'CheckFailure' to 'CompatGHC.Messages' so we can hand off to GHC printing mechanism
 in the plugin.
-}
errorMessagesFromList ::
  CompatGHC.Bag CheckFailureWithNote -> CompatGHC.Messages CheckFailureWithNote
errorMessagesFromList =
  CompatGHC.mkMessages . fmap mkEnv
{-# INLINEABLE errorMessagesFromList #-}

checkFailureImport :: CheckFailure -> CodeStructure.Import
checkFailureImport (DependencyViolation i _) = i
checkFailureImport (EncapsulationViolation i _) = i
checkFailureImport (QualificationViolation i _) = i
checkFailureImport (OpenImportViolation _ (i NEL.:| _)) = i
checkFailureImport (AliasUniquenessViolation (i NEL.:| _)) = i

checkFailureLoc :: CheckFailure -> CompatGHC.SrcSpan
checkFailureLoc = CodeStructure.srcLocation . checkFailureImport

{- | The only part of the 'CompatGHC.Diagnostic' class that we really care about is the
 'diagnosticMessage', used for printing.
-}
instance CompatGHC.Diagnostic CheckFailureWithNote where
  type DiagnosticOpts (Rules.FailureWithUserNote CheckFailure) = CompatGHC.NoDiagnosticOpts
  diagnosticMessage _ = CompatGHC.mkSimpleDecorated . CompatGHC.ppr
  diagnosticReason = const CompatGHC.ErrorWithoutFlag
  diagnosticHints = const []
  diagnosticCode =
    Just . checkFailureDiagnosticCode . Rules.underlyingFailure

mkEnv :: CheckFailureWithNote -> CompatGHC.MsgEnvelope (Rules.FailureWithUserNote CheckFailure)
mkEnv cf =
  CompatGHC.mkErrorMsgEnvelope
    (checkFailureLoc $ Rules.underlyingFailure cf)
    CompatGHC.neverQualify
    cf

formatDependencyViolation :: CodeStructure.Import -> CheckedDependency -> CompatGHC.SDoc
formatDependencyViolation imp dep =
  CompatGHC.sep
    [ CompatGHC.hsep
        [ formatImportSubject imp
        , CompatGHC.text "is forbidden by the declaration that the module tree"
        , CompatGHC.ppr $ dependencySource dep
        , CompatGHC.text "depends on"
        , CompatGHC.ppr $ dependencyTarget dep
        ]
    , CompatGHC.blankLine
    ]

formatEncapsulationViolation :: CodeStructure.Import -> CodeStructure.ModuleTree -> CompatGHC.SDoc
formatEncapsulationViolation imp moduleTree =
  CompatGHC.sep
    [ CompatGHC.sep
        [ formatImportSubject imp
        , CompatGHC.text "is forbidden because it is an internal module of the encapsulated tree"
        , CompatGHC.ppr moduleTree
        ]
    , CompatGHC.blankLine
    ]

formatImportSubject :: CodeStructure.Import -> CompatGHC.SDoc
formatImportSubject imp =
  CompatGHC.hsep
    [ CompatGHC.text "The import of"
    , CompatGHC.ppr (CodeStructure.importedModule imp)
    ]

formatQualificationViolation :: CodeStructure.Import -> [CodeStructure.Scheme] -> CompatGHC.SDoc
formatQualificationViolation imp alloweds =
  let rebuildFromImport =
        rebuildImportStatementFromScheme (CodeStructure.importedModule imp)
      beginningDoc =
        CompatGHC.hang
          ( CompatGHC.sep
              [ CompatGHC.hsep
                  [ formatImportSubject imp
                  , CompatGHC.text "is improper because it does not match one of the allowed import schemes."
                  ]
              , CompatGHC.cat
                  [ CompatGHC.text "It was imported"
                  , CompatGHC.colon
                  ]
              ]
          )
          4
          (rebuildFromImport . CodeStructure.buildScheme . CompatGHC.unLoc $ CodeStructure.importDecl imp)
      endDoc =
        CompatGHC.hang
          ( CompatGHC.cat
              [ CompatGHC.text "But it may only be imported in the followings ways"
              , CompatGHC.colon
              ]
          )
          4
          (CompatGHC.vcat $ fmap rebuildFromImport alloweds)
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        , endDoc
        , CompatGHC.blankLine
        ]

formatOpenImportViolation ::
  NEL.NonEmpty CodeStructure.Import -> Rules.MaximumNat -> CompatGHC.SDoc
formatOpenImportViolation imps maxAllowedNat =
  let rebuildFromImport imp =
        rebuildImportStatementFromScheme (CodeStructure.importedModule imp)
          . CodeStructure.buildScheme
          . CompatGHC.unLoc
          $ CodeStructure.importDecl imp
      beginningDoc =
        CompatGHC.hang
          ( CompatGHC.sep
              [ CompatGHC.hsep
                  [ CompatGHC.text "There were too many open imports. The max allowed is:"
                  , CompatGHC.ppr maxAllowedNat
                  ]
              , CompatGHC.cat
                  [ CompatGHC.text "The open imports are"
                  , CompatGHC.colon
                  ]
              ]
          )
          4
          (CompatGHC.vcat . fmap rebuildFromImport $ NEL.toList imps)
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

formatAliasUniquenssViolation :: NEL.NonEmpty CodeStructure.Import -> CompatGHC.SDoc
formatAliasUniquenssViolation imps =
  let rebuildFromImport imp =
        rebuildImportStatementFromScheme (CodeStructure.importedModule imp)
          . CodeStructure.buildScheme
          . CompatGHC.unLoc
          $ CodeStructure.importDecl imp
      beginningDoc =
        CompatGHC.hang
          ( CompatGHC.sep
              [ CompatGHC.hsep
                  [ CompatGHC.text "Multiple imports share an alias that must be unique."
                  ]
              , CompatGHC.cat
                  [ CompatGHC.text "The imports are"
                  , CompatGHC.colon
                  ]
              ]
          )
          4
          (CompatGHC.vcat . fmap rebuildFromImport $ NEL.toList imps)
   in CompatGHC.vcat
        [ beginningDoc
        , CompatGHC.blankLine
        ]

{- | Rebuild the part of an import statement we subject to rules, namely everything up to explicit
 items imported or hiding
-}
rebuildImportStatementFromScheme :: CompatGHC.ModuleName -> CodeStructure.Scheme -> CompatGHC.SDoc
rebuildImportStatementFromScheme modName schema =
  CompatGHC.hsep
    [ CompatGHC.dot
    , CompatGHC.hsep
        [ CompatGHC.text "import"
        , if CodeStructure.safeToBool (CodeStructure.safe schema)
            then
              CompatGHC.text "safe"
            else
              -- Purposely do not mention the safe keyword when not forced, as
              -- this makes the output needlessly complex and we don't need to
              -- mention something requiring an extension if users didn't ask for
              -- it explicitly.
              CompatGHC.empty
        , case CodeStructure.qualification schema of
            CompatGHC.QualifiedPre -> CompatGHC.text "qualified"
            CompatGHC.QualifiedPost -> CompatGHC.empty
            CompatGHC.NotQualified -> CompatGHC.empty
        , case CodeStructure.packageQualification schema of
            -- The order here is very important, as package qualified imports must come after safe
            -- and prepositive qualified.
            CodeStructure.WithoutPackageQualifier ->
              CompatGHC.empty
            CodeStructure.WithPackageQualifier str ->
              CompatGHC.doubleQuotes $ CompatGHC.text (T.unpack str)
        , CompatGHC.ppr modName
        , case CodeStructure.qualification schema of
            CompatGHC.QualifiedPre -> CompatGHC.empty
            CompatGHC.QualifiedPost -> CompatGHC.text "qualified"
            CompatGHC.NotQualified -> CompatGHC.empty
        , case CodeStructure.alias schema of
            CodeStructure.WithoutAlias ->
              CompatGHC.empty
            CodeStructure.WithAlias name ->
              CompatGHC.hsep
                [ CompatGHC.text "as"
                , CompatGHC.ppr name
                ]
        ]
    ]
