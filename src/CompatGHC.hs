{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-methods #-}
#if __GLASGOW_HASKELL__ == 904
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

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
  , IE (..)
  , ImportDecl
  , ImportDeclQualifiedStyle (..)
  , LIE
  , LImportDecl
  , ModSummary (..)
  , ModuleName
  , SrcSpan
  , generatedSrcSpan
  , getLoc
  , ideclAs
  , ideclName
  , ideclPkgQual
  , ideclQualified
  , ideclSafe
  , ideclImportList
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
  , TcGblEnv (tcg_rn_exports, tcg_rn_imports, tcg_mod, tcg_doc_hdr)
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
  , unitIdString
  , neverQualify
  , keepRenamedSource
  -- GHC.Types.Error
  , Diagnostic (..)
  , MsgEnvelope
  , mkSimpleDecorated
  , NoDiagnosticOpts (NoDiagnosticOpts)
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
  , IE (..)
  , ImportDecl
  , ImportDeclQualifiedStyle (..)
  , LIE
  , LImportDecl
  , ModSummary (..)
  , ModuleName
  , PkgQual (..)
  , SrcSpan
  , getLoc
  , ideclAs
  , ideclName
  , ideclPkgQual
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
  , generatedSrcSpan
  , hang
  , hsep
  , keepRenamedSource
  , liftIO
  , neverQualify
  , purePlugin
  , sep
  , text
  , unitIdString
  , vcat
  )
import GHC.Tc.Utils.Monad (TcGblEnv (tcg_mod, tcg_rn_imports), TcM)
import qualified GHC.Tc.Utils.Monad as GHC
import qualified GHC.Types.Error as GHC

import Data.Typeable (Typeable)
import GHC.Plugins (DiagnosticReason (..), Messages)
import qualified GHC.Tc.Errors.Types as GHC
import GHC.Types.Error (MsgEnvelope (..), mkSimpleDecorated)

#if __GLASGOW_HASKELL__ == 904
import qualified GHC
#endif

#if __GLASGOW_HASKELL__ == 906
import GHC (ideclImportList)
import GHC.Types.Error (Diagnostic(..), NoDiagnosticOpts(NoDiagnosticOpts))
import GHC.Utils.Error (mkErrorMsgEnvelope)
#endif

#if __GLASGOW_HASKELL__ == 908
import GHC (ideclImportList)
import GHC.Types.Error (Diagnostic(..), NoDiagnosticOpts(NoDiagnosticOpts))
import GHC.Utils.Error (mkErrorMsgEnvelope)
#endif

#if __GLASGOW_HASKELL__ == 904

data NoDiagnosticOpts = NoDiagnosticOpts

-- | The 'Diagnostic' class has seen a frustrating amount of churn, changing in every release, here
-- we model the 9.8 interface, but just enough for what we need.
class Diagnostic a where
  type DiagnosticOpts a
  diagnosticMessage :: Int -> a -> GHC.DecoratedSDoc
  diagnosticReason  :: a -> DiagnosticReason
  diagnosticHints   :: a -> [b]
  diagnosticCode :: a -> Maybe b

instance Diagnostic a => GHC.Diagnostic a where
  diagnosticMessage = diagnosticMessage 0
  diagnosticReason = diagnosticReason
  diagnosticHints = diagnosticHints

-- | Compatibility shim as this datatype was added in GHC 9.6, along with 'ideclImportList'         │
data ImportListInterpretation = Exactly | EverythingBut

-- | Compatibility shim as GHC 9.4 used 'ideclHiding', but later changed to 'ideclImportList'.      │
ideclImportList :: ImportDecl pass -> Maybe (ImportListInterpretation, GHC.XRec pass [GHC.XRec pass (GHC.IE pass)])
ideclImportList idecl =
  case GHC.ideclHiding idecl of
    Nothing -> Nothing
    Just (True, n) -> Just (EverythingBut,n)
    Just (False,n) -> Just (Exactly,n)

-- | Helper for creating a 'MsgEnvelope' as needed for 9.4.x
mkErrorMsgEnvelope :: SrcSpan -> unused -> e -> MsgEnvelope e
mkErrorMsgEnvelope msgSpan _ a =
  MsgEnvelope
    { errMsgSpan = msgSpan
    , errMsgContext = neverQualify
    , errMsgSeverity = GHC.SevError
    , errMsgDiagnostic = a
    }

-- | Helper to add messages to the type checking monad so our plugin will print our output and fail
-- a build.
addMessages :: (Typeable a, GHC.Diagnostic a) => Messages a -> TcM ()
addMessages =
  GHC.addMessages . fmap GHC.TcRnUnknownMessage

#endif

#if __GLASGOW_HASKELL__ == 906

-- | Helper to add messages to the type checking monad so our plugin will print our output and fail
-- a build.
addMessages ::
  (Typeable a
  , GHC.Diagnostic a
  , GHC.DiagnosticOpts a ~ GHC.NoDiagnosticOpts
  ) =>
  Messages a ->
  TcM ()
addMessages =
  GHC.addMessages . fmap GHC.mkTcRnUnknownMessage

#endif

#if __GLASGOW_HASKELL__ == 908

-- | Helper to add messages to the type checking monad so our plugin will print our output and fail
-- a build.
addMessages ::
  ( Typeable a
  , Diagnostic a
  , GHC.DiagnosticOpts a ~ GHC.NoDiagnosticOpts
  ) =>
  Messages a ->
  TcM ()
addMessages =
  GHC.addMessages . fmap GHC.mkTcRnUnknownMessage

#endif

-- | Helper for creating 'Messages'
mkMessagesFromList :: [MsgEnvelope e] -> Messages e
mkMessagesFromList = GHC.mkMessages . GHC.listToBag

-- -- | Dhall decoder for 'ModuleName'
moduleNameDecoder :: Dhall.Decoder ModuleName
moduleNameDecoder =
  fmap mkModuleName Dhall.string

-- -- | Dhall decoder for qualification style so that it can be read from configuration
qualificationDecoder :: Dhall.Decoder GHC.ImportDeclQualifiedStyle
qualificationDecoder =
  Dhall.union $
    (const GHC.QualifiedPre <$> Dhall.constructor (T.pack "QualifiedPre") Dhall.unit)
      <> (const GHC.NotQualified <$> Dhall.constructor (T.pack "Unqualified") Dhall.unit)
      <> (const GHC.QualifiedPost <$> Dhall.constructor (T.pack "QualifiedPost") Dhall.unit)
