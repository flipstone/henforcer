{-# LANGUAGE Strict #-}

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
import qualified System.IO.Unsafe as UnsafeIO

import qualified CompatGHC
import qualified Henforcer.Checks as Checks
import qualified Henforcer.Config as Config
import qualified Henforcer.Options as Options

import qualified Pollock

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
    , CompatGHC.renamedResultAction = CompatGHC.keepRenamedSource
    , CompatGHC.typeCheckResultAction = typeCheckResultAction
    , CompatGHC.driverPlugin = Pollock.ensureHaddockIsOn
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
  config <- CompatGHC.liftIO $ loadConfigIfNeeded commandLineOpts
  let
    modName = CompatGHC.moduleName $ CompatGHC.tcg_mod tcGblEnv

  CompatGHC.addMessages
    . Checks.errorMessagesFromList
    $ Checks.checkImports (Checks.determineChecks config modName) tcGblEnv

  pollockModuleInfo <- CompatGHC.liftIO $ Pollock.processModule tcGblEnv

  CompatGHC.addMessages
    . Checks.docErrorMessagesFromList
    $ Checks.checkDocumentation (Checks.determineDocumentationChecks config modName) pollockModuleInfo

  pure tcGblEnv

{- | Determines if recompilation of previous modules should happen. This always gets the fingerprint
of the configuration so that a cached version does not conflict with the recompilation checks.
-}
recompile :: [CompatGHC.CommandLineOption] -> IO CompatGHC.PluginRecompile
recompile =
  fmap (CompatGHC.MaybeRecompile . snd) . getConfigPathAndFingerprint

-- | Load the config into the global state, if it isn't there.
loadConfigIfNeeded :: [CompatGHC.CommandLineOption] -> IO Config.Config
loadConfigIfNeeded commandLineOpts = do
  mbConfigWithFingerprint <- MVar.tryReadMVar globalConfigState
  case mbConfigWithFingerprint of
    Just (conf, mvarFingerprint) -> do
      (configPath, currentFingerprint) <- getConfigPathAndFingerprint commandLineOpts
      -- A small optimization here, if the fingerprint is unchanged we do not need to reload the
      -- config. If it has changed then make sure to reload the config.
      if currentFingerprint == mvarFingerprint
        then pure conf
        else do
          loadedConfig <- Config.loadConfig configPath
          _ <- MVar.tryPutMVar globalConfigState (loadedConfig, currentFingerprint)
          pure loadedConfig
    Nothing -> do
      (configPath, currentFingerprint) <- getConfigPathAndFingerprint commandLineOpts
      loadedConfig <- Config.loadConfig configPath
      -- If we've been beaten to filling the global config, then oh well, but we do not want to
      -- block on it.
      _ <- MVar.tryPutMVar globalConfigState (loadedConfig, currentFingerprint)
      pure loadedConfig

{- | Parse the command line options for the path to the config and compute the fingerprint of that
config.
-}
getConfigPathAndFingerprint :: [CompatGHC.CommandLineOption] -> IO (FilePath, CompatGHC.Fingerprint)
getConfigPathAndFingerprint commandLineOpts = do
  configPath <- fmap Options.configPath $ Options.parseGivenOptions commandLineOpts
  fingerprint <- CompatGHC.getFileHash configPath
  pure (configPath, fingerprint)
