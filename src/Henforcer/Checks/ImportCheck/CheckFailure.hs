{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{- |
Module      : Henforcer.Checks.ImportCheck.CheckFailure
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Checks.ImportCheck.CheckFailure
  ( CheckFailure (..)
  , CheckedDependency (..)
  , errorMessagesFromList
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T

import qualified CompatGHC
import qualified Henforcer.CodeStructure as CodeStructure

data CheckedDependency = CheckedDependency
  { dependencySource :: !CodeStructure.TreeName
  , dependencyTarget :: !CodeStructure.TreeName
  }

data CheckFailure
  = DependencyViolation !CodeStructure.Import !CheckedDependency
  | EncapsulationViolation !CodeStructure.Import !CodeStructure.TreeName
  | QualificationViolation !CodeStructure.Import ![CodeStructure.Scheme]
  | OpenImportViolation !(NEL.NonEmpty CodeStructure.Import) !CodeStructure.MaxOpenUnaliasedImportsNat
  | AliasUniquenessViolation !(NEL.NonEmpty CodeStructure.Import)

instance CompatGHC.Outputable CheckFailure where
  ppr cf =
    case cf of
      DependencyViolation i cd -> formatDependencyViolation i cd
      EncapsulationViolation i tn -> formatEncapsulationViolation i tn
      QualificationViolation i s -> formatQualificationViolation i s
      OpenImportViolation i n -> formatOpenImportViolation i n
      AliasUniquenessViolation is -> formatAliasUniquenssViolation is

{- | Convert a list of 'CheckFailure' to 'CompatGHC.Messages' so we can hand off to GHC printing mechanism
 in the plugin.
-}
errorMessagesFromList :: [CheckFailure] -> CompatGHC.Messages CheckFailure
errorMessagesFromList =
  CompatGHC.mkMessagesFromList . fmap mkEnv

checkFailureImport :: CheckFailure -> CodeStructure.Import
checkFailureImport (DependencyViolation i _) = i
checkFailureImport (EncapsulationViolation i _) = i
checkFailureImport (QualificationViolation i _) = i
checkFailureImport (OpenImportViolation (i NEL.:| _) _) = i
checkFailureImport (AliasUniquenessViolation (i NEL.:| _)) = i

checkFailureLoc :: CheckFailure -> CompatGHC.SrcSpan
checkFailureLoc = CodeStructure.srcLocation . checkFailureImport

{- | The only part of the 'CompatGHC.Diagnostic' class that we really care about is the
 'diagnosticMessage', used for printing.
-}
instance CompatGHC.Diagnostic CheckFailure where
  type DiagnosticOpts CheckFailure = CompatGHC.NoDiagnosticOpts
  diagnosticMessage _ = CompatGHC.mkSimpleDecorated . CompatGHC.ppr
  diagnosticReason = const CompatGHC.ErrorWithoutFlag
  diagnosticHints = const []
  diagnosticCode = const Nothing

mkEnv :: CheckFailure -> CompatGHC.MsgEnvelope CheckFailure
mkEnv cf =
  CompatGHC.mkErrorMsgEnvelope (checkFailureLoc cf) CompatGHC.neverQualify cf

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

formatEncapsulationViolation :: CodeStructure.Import -> CodeStructure.TreeName -> CompatGHC.SDoc
formatEncapsulationViolation imp treeName =
  CompatGHC.sep
    [ CompatGHC.sep
        [ formatImportSubject imp
        , CompatGHC.text "is forbidden because it is an internal module of the encapsulated tree"
        , CompatGHC.ppr treeName
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
  NEL.NonEmpty CodeStructure.Import -> CodeStructure.MaxOpenUnaliasedImportsNat -> CompatGHC.SDoc
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
        , case CodeStructure.safe schema of
            CodeStructure.WithoutSafe -> CompatGHC.empty -- Purposely do not mention the safe keyword when not forced, as
            -- this makes the output needlessly complex and we don't need to
            -- mention something requiring an extension if users didn't ask for
            -- it explicitly.
            CodeStructure.WithSafe -> CompatGHC.text "safe"
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
            CompatGHC.QualifiedPost -> CompatGHC.text "qualifed"
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
