{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : CompatGHC
Description : This collects all of the imports from GHC to limit the need to handle multiple versions to only this module. The intent is for the rest of the code to never import from GHC directly but use this as the interface. As such this both re-exports from GHC, and creates some helper functions to generally ease development.
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module CompatGHC
  ( -- GHC
    GhcRn
  , ImportDecl
  , ImportDeclQualifiedStyle (..)
  , LImportDecl
  , ModSummary (..)
  , ModuleName
  , SrcSpan
  , getLoc
  , ideclAs
  , ideclName
  , ideclPkgQual
  , ideclQualified
  , ideclSafe
  , ideclHiding
  , locA
  , moduleName
  , moduleNameString
  , unLoc
  -- GHC.Fingerprint
  , Fingerprint
  , getFileHash
  -- GHC.Plugins
  , CommandLineOption
  , DiagnosticReason (..)
  , Messages
  , Outputable (ppr)
  , Plugin (..)
  , PluginRecompile (MaybeRecompile)
  , SDoc
  , TcGblEnv (tcg_rn_imports, tcg_mod)
  , TcM
  , UnitId (..)
  , blankLine
  , cat
  , colon
  , defaultPlugin
  , dot
  , doubleQuotes
  , empty
  , hang
  , hsep
  , liftIO
  , purePlugin
  , sep
  , text
  , vcat
  -- GHC.Types.Error
  , Diagnostic (..)
  , MsgEnvelope
  , mkSimpleDecorated
  -- internal defined helpers
  , PkgQual (..)
  , addMessages
  , mkMessagesFromList
  , mkErrorMsgEnvelope
  , moduleNameDecoder
  , qualificationDecoder
  ) where

import qualified Data.Text as T
import qualified Dhall
import GHC
  ( GhcRn
  , ImportDecl
  , ImportDeclQualifiedStyle (..)
  , LImportDecl
  , ModSummary (..)
  , ModuleName
  , SrcSpan
  , getLoc
  , ideclAs
  , ideclHiding
  , ideclName
  , ideclQualified
  , ideclSafe
  , locA
  , mkModuleName
  , moduleName
  , moduleNameString
  , unLoc
  )
import qualified GHC.Data.Bag as GHC
import GHC.Fingerprint (Fingerprint, getFileHash)
import qualified GHC.Hs as GHC
import GHC.Plugins
  ( CommandLineOption
  , Outputable (ppr)
  , Plugin (..)
  , PluginRecompile (MaybeRecompile)
  , SDoc
  , UnitId (..)
  , blankLine
  , cat
  , colon
  , defaultPlugin
  , dot
  , doubleQuotes
  , empty
  , hang
  , hsep
  , liftIO
  , purePlugin
  , sep
  , text
  , vcat
  )
import qualified GHC.Plugins as GHC
import GHC.Tc.Utils.Monad (TcGblEnv (tcg_mod, tcg_rn_imports), TcM)
import qualified GHC.Tc.Utils.Monad as GHC
import qualified GHC.Types.Error as GHC

#if __GLASGOW_HASKELL__ == 902
import GHC.Types.Error(Messages, MsgEnvelope(..))
import qualified GHC.Types.SourceText as GHC902

#else

import Data.Typeable (Typeable)
import GHC.Plugins (DiagnosticReason(..), Messages)
import qualified GHC.Tc.Errors.Types as GHC904
import GHC.Types.Error (MsgEnvelope(..), Diagnostic(..), mkSimpleDecorated)

#endif

#if __GLASGOW_HASKELL__ == 902

-- | The bare minimum of the GHC 9.4 'DiagnosticReason' type that we need to support in 9.2
data DiagnosticReason = ErrorWithoutFlag

-- While we never create any values of this type, it allows us to keep the 'Diagnostic' interface
-- the same as in 9.4
data GhcHint

-- | The bare minimum of the GHC 9.4 'Diagnostic' class as needed to support 9.2
class Diagnostic a where
  diagnosticMessage :: a -> GHC.DecoratedSDoc
  diagnosticReason  :: a -> DiagnosticReason
  diagnosticHints   :: a -> [GhcHint]

instance {-# INCOHERENT #-} (Diagnostic a) => GHC.RenderableDiagnostic a where
  renderDiagnostic = diagnosticMessage

-- | 'mkSimpleDecorated' compat as needed for 9.2.x
mkSimpleDecorated :: SDoc -> GHC.DecoratedSDoc
mkSimpleDecorated = GHC.mkDecorated . pure

-- | Helper for creating a 'MsgEnvelope' as needed for 9.2.x
mkErrorMsgEnvelope :: SrcSpan -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan =
  GHC.mkErr msgSpan GHC.neverQualify

-- | Helper to add messages to the type checking monad so our plugin will print our output and fail
-- a build.
addMessages :: (Diagnostic a) => Messages a -> TcM ()
addMessages =
  GHC.addMessages . fmap diagnosticMessage

-- | Helper that purposely shadows the name provided in GHC. The api from GHC changed somewhat
-- significantly in 9.4. Once support for 9.2 is dropped this can be removed as a general cleanup
-- step with minimal changes.
data PkgQual =
  NotPkgQualified
  | PkgQualified String

-- | Helper that purposely shadows the name provided in GHC. The api from GHC changed somewhat
-- significantly in 9.4. Once support for 9.2 is dropped this can be removed as a general cleanup
-- step with minimal changes.
ideclPkgQual :: ImportDecl GhcRn -> PkgQual
ideclPkgQual i =
  case GHC.ideclPkgQual i of
    Nothing -> NotPkgQualified
    Just fs -> PkgQualified (GHC.unpackFS $ GHC902.sl_fs fs)

#endif

#if __GLASGOW_HASKELL__ == 904

-- | Helper for creating a 'MsgEnvelope' as needed for 9.4.x
mkErrorMsgEnvelope :: SrcSpan -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan a =
  MsgEnvelope
    { errMsgSpan = msgSpan
    , errMsgContext = GHC.neverQualify
    , errMsgSeverity = GHC.SevError
    , errMsgDiagnostic = a
    }

-- | Helper to add messages to the type checking monad so our plugin will print our output and fail
-- a build.
addMessages :: (Typeable a, Diagnostic a) => Messages a -> TcM ()
addMessages =
  GHC.addMessages . fmap GHC904.TcRnUnknownMessage

-- | Helper that purposely shadows the name provided in GHC. The api from GHC changed somewhat
-- significantly in 9.4. Once support for 9.2 is dropped this can be removed as a general cleanup
-- step with minimal changes.
data PkgQual =
  NotPkgQualified
  | PkgQualified String

-- | Helper that purposely shadows the name provided in GHC. The api from GHC changed somewhat
-- significantly in 9.4. Once support for 9.2 is dropped this can be removed as a general cleanup
-- step with minimal changes.
ideclPkgQual :: ImportDecl GhcRn -> PkgQual
ideclPkgQual i =
  case GHC.ideclPkgQual i of
    GHC.NoPkgQual -> NotPkgQualified
    GHC.ThisPkg u -> PkgQualified (GHC.unitIdString u)
    GHC.OtherPkg u -> PkgQualified (GHC.unitIdString u)

#endif

-- | Helper for creating 'Messages'
mkMessagesFromList :: [MsgEnvelope e] -> Messages e
mkMessagesFromList = GHC.mkMessages . GHC.listToBag

-- | Dhall decoder for 'ModuleName'
moduleNameDecoder :: Dhall.Decoder ModuleName
moduleNameDecoder =
  fmap mkModuleName Dhall.string

-- | Dhall decoder for qualification style so that it can be read from configuration
qualificationDecoder :: Dhall.Decoder GHC.ImportDeclQualifiedStyle
qualificationDecoder =
  Dhall.union $
    (const GHC.QualifiedPre <$> Dhall.constructor (T.pack "QualifiedPre") Dhall.unit)
      <> (const GHC.NotQualified <$> Dhall.constructor (T.pack "Unqualified") Dhall.unit)
      <> (const GHC.QualifiedPost <$> Dhall.constructor (T.pack "QualifiedPost") Dhall.unit)
