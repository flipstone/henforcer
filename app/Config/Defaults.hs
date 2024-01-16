{-# LANGUAGE TemplateHaskell #-}

module Config.Defaults
  ( initialHenforcerFile
  ) where

import qualified Data.ByteString as BS
import qualified Data.FileEmbed as FileEmbed

initialHenforcerFile :: BS.ByteString
initialHenforcerFile =
  $(FileEmbed.embedFile "data/henforcer.toml")
