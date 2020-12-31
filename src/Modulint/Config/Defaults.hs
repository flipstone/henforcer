{-# LANGUAGE TemplateHaskell #-}
module Modulint.Config.Defaults
  ( packageFile
  , initialModulintFile
  ) where

import qualified Data.ByteString as BS
import qualified Data.FileEmbed as FileEmbed

packageFile :: BS.ByteString
packageFile =
  $(FileEmbed.embedFile "src/Modulint/Config/package.dhall")

initialModulintFile :: BS.ByteString
initialModulintFile =
  $(FileEmbed.embedFile "src/Modulint/Config/modulint.dhall")
