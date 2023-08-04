{- |
Module      : Henforcer
Description :
Copyright   : (c) Flipstone Technology Partners, 2023
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer
  ( plugin
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad as M
import qualified System.IO.Unsafe as UnsafeIO

import qualified CompatGHC
import qualified Henforcer.Checks as Checks
import qualified Henforcer.Config as Config
import qualified Henforcer.Options as Options

-- Using an MVar and unsafePerformIO here is a very ugly hack, but the plugin interface gives us no
-- way to load the configuration once for the entire set of modules being compiled. This is a big
-- enough performance win that the cost seems likely worth it otherwise.
globalConfigState :: MVar.MVar (Config.Config, CompatGHC.Fingerprint)
{-# NOINLINE globalConfigState #-}
globalConfigState = UnsafeIO.unsafePerformIO MVar.newEmptyMVar

plugin :: CompatGHC.Plugin
plugin =
  CompatGHC.defaultPlugin
    { CompatGHC.pluginRecompile = recompile
    , CompatGHC.typeCheckResultAction = typeCheckResultAction
    }

{- | During typechecking is when Henforcer performs checks, adding any violations to the error
messages tracked by GHC.
-}
typeCheckResultAction ::
  [CompatGHC.CommandLineOption]
  -> CompatGHC.ModSummary
  -> CompatGHC.TcGblEnv
  -> CompatGHC.TcM CompatGHC.TcGblEnv
typeCheckResultAction commandLineOpts _modSummary tcGblEnv = do
  let getImportChecker = fmap (Checks.newImportChecker . fst) . loadConfigIfNeeded
  importChecker <- CompatGHC.liftIO $ getImportChecker commandLineOpts

  CompatGHC.addMessages
    . Checks.errorMessagesFromList
    $ Checks.checkModule importChecker tcGblEnv

  pure tcGblEnv

{- | Determines if recompilation of previous modules should happen. This always reloads the
configuration so that a cached version does not conflict with the recompilation checks.
-}
recompile :: [CompatGHC.CommandLineOption] -> IO CompatGHC.PluginRecompile
recompile =
  fmap (CompatGHC.MaybeRecompile . snd) . loadConfigFileFromOpts

-- | Load the config into the global state, if it isn't there.
loadConfigIfNeeded :: [CompatGHC.CommandLineOption] -> IO (Config.Config, CompatGHC.Fingerprint)
loadConfigIfNeeded commandLineOpts = do
  mbConfigWithFingerprint <- MVar.tryReadMVar globalConfigState
  case mbConfigWithFingerprint of
    Just c ->
      pure c
    Nothing -> do
      configWithFingerprint <- loadConfigFileFromOpts commandLineOpts
      -- If we've been beaten to filling the global config, then oh well, but we do not want to
      -- block on it.
      _ <- MVar.tryPutMVar globalConfigState configWithFingerprint
      pure configWithFingerprint

-- | Parse options to get configuration path and load the config.
loadConfigFileFromOpts :: [String] -> IO (Config.Config, CompatGHC.Fingerprint)
loadConfigFileFromOpts =
  M.join
    . fmap (Config.loadConfigFileWithFingerprint . Options.configPath)
    . Options.parseGivenOptions
