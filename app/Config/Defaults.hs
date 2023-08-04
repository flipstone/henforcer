{-# LANGUAGE TemplateHaskell #-}

module Config.Defaults
  ( packageFile
  , initialHenforcerFile
  ) where

import qualified Data.ByteString as BS
import qualified Data.FileEmbed as FileEmbed

packageFile :: BS.ByteString
packageFile =
  $(FileEmbed.embedFile "data/package.dhall")

initialHenforcerFile :: BS.ByteString
initialHenforcerFile =
  $(FileEmbed.embedFile "data/henforcer.dhall")
