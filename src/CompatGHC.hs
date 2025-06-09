{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-methods #-}
#if __GLASGOW_HASKELL__ == 904
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#else
{-# LANGUAGE TypeOperators #-}
#endif

{- |
Module      : CompatGHC
Description : This collects all of the imports from GHC to limit the need to handle multiple versions to only this module. The intent is for the rest of the code to never import from GHC directly but use this as the interface. As such this both re-exports from GHC, and creates some helper functions to generally ease development.
Copyright   : (c) Flipstone Technology Partners, 2023-2025
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
  , mkModuleName
  , moduleName
  , moduleNameString
  , unLoc
  -- GHC.Data.Bag
  , Bag
  , listToBag
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
  , TcGblEnv (tcg_rn_exports, tcg_rn_imports, tcg_mod)
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
  , DiagnosticCode (..)
  , MsgEnvelope
  , mkSimpleDecorated
  , mkDecorated
  , NoDiagnosticOpts (NoDiagnosticOpts)
  , mkMessages
  -- internal defined helpers
  , PkgQual (..)
  , addMessages
  , mkErrorMsgEnvelope
  , moduleNameCodec
  , moduleNameListCodec
  , mkHenforcerDiagnosticCode
  ) where

import qualified Data.Text as T
import Data.Typeable (Typeable)
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
import GHC.Data.Bag (Bag, listToBag)
import qualified GHC.Data.Bag as GHC
import GHC.Fingerprint (Fingerprint, getFileHash)
import GHC.Plugins
  ( CommandLineOption
  , DiagnosticReason (..)
  , Messages
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
import qualified GHC.Tc.Errors.Types as GHC
import GHC.Tc.Utils.Monad (TcGblEnv (tcg_mod, tcg_rn_imports), TcM)
import qualified GHC.Tc.Utils.Monad as GHC
import GHC.Types.Error (MsgEnvelope (..), mkDecorated, mkMessages, mkSimpleDecorated)
import qualified GHC.Types.Error as GHC
import qualified Numeric.Natural as Natural
import qualified Toml

#if __GLASGOW_HASKELL__ == 904
import qualified GHC
import qualified Text.Printf as Printf
#endif

#if __GLASGOW_HASKELL__ >= 906 && __GLASGOW_HASKELL__ <= 912
import GHC (ideclImportList)
import GHC.Types.Error (Diagnostic(..), NoDiagnosticOpts(NoDiagnosticOpts), DiagnosticCode(..))
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
  diagnosticCode :: a -> Maybe DiagnosticCode

-- | The actual printed form for Diagnostics in 9.4 is not exactly the same as in later versions
-- because we would have to hack in the printing of the diagnostic code, and do not have a good way
-- to make it appear the same as later versions so we omit it.  This has the advantage of being
-- consistent with 9.4 not printing diagnostic codes from GHC itself, at the cost of less
-- information to our users.
instance Diagnostic a => GHC.Diagnostic a where
  diagnosticMessage = diagnosticMessage 0
  diagnosticReason = diagnosticReason
  diagnosticHints = diagnosticHints

-- | Compatibility shim as this datatype was added in GHC 9.6, which is when the error codes first started being reported.
data DiagnosticCode =
  DiagnosticCode
    { diagnosticCodeNameSpace :: String
    , diagnosticCodeNumber    :: Natural.Natural
    }

-- | The instances for DiagnosticCode are made to be consistent with later GHC versions
instance Show DiagnosticCode where
  show (DiagnosticCode prefix c) =
    prefix <> "-" <> Printf.printf "%05d" c
      -- Newer GHC that include 'DiagnosticCode' pad to 5 digits. So we do the same for consistency.

instance Outputable DiagnosticCode where
  ppr code = text (show code)

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

#if __GLASGOW_HASKELL__ >= 906
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

{- | Toml codec for 'ModuleName'. Note that the encode here may not be what is desired, but
Henforcer does not, as of this writing, actually create Toml, just consume it.
-}
moduleNameCodec :: Toml.Key -> Toml.TomlCodec ModuleName
moduleNameCodec =
  Toml.dimap show mkModuleName . Toml.string
{-# INLINEABLE moduleNameCodec #-}

moduleNameListCodec :: Toml.Key -> Toml.TomlCodec [ModuleName]
moduleNameListCodec =
  Toml.arrayOf (Toml._TextBy (T.pack . show) (pure . mkModuleName . T.unpack))
{-# INLINEABLE moduleNameListCodec #-}

instance Show ImportDeclQualifiedStyle where
  show = declStyleToStr

declStyleToStr :: ImportDeclQualifiedStyle -> String
declStyleToStr decl =
  case decl of
    QualifiedPre -> "QualifiedPre"
    QualifiedPost -> "QualifiedPost"
    NotQualified -> "NotQualified"

instance Applicative Bag where
  pure = GHC.unitBag

-- | A helper to create 'DiagnosticCode' while ensuring we _always_ use the correct prefix.
mkHenforcerDiagnosticCode :: Natural.Natural -> DiagnosticCode
mkHenforcerDiagnosticCode =
  DiagnosticCode "HEN"
