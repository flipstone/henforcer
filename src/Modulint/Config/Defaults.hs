{-# LANGUAGE TemplateHaskell #-}
module Modulint.Config.Defaults
  ( preludeFile
  , initialModulintFile
  ) where

import qualified Data.ByteString as BS
import qualified Data.FileEmbed as FileEmbed

preludeFile :: BS.ByteString
preludeFile =
  $(FileEmbed.embedFile "src/Modulint/Config/prelude.dhall")

initialModulintFile :: BS.ByteString
initialModulintFile =
  $(FileEmbed.embedFile "src/Modulint/Config/modulint.dhall")
