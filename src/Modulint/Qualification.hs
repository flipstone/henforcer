module Modulint.Qualification
  ( AllowedMap
  , Qualification(..)
  , Alias(..)
  , AllowedQualification(..)
  , lookupAllowed
  ) where

import qualified Data.Map.Strict as Map

import qualified Modulint.ModuleName as ModuleName

type AllowedMap =
  Map.Map ModuleName.ModuleName [AllowedQualification]

data Qualification
  = Qualified
  | Unqualified
  deriving Show

data Alias
  = WithAlias ModuleName.ModuleName
  | WithoutAlias
  deriving Show

data AllowedQualification =
  AllowedQualification
    { qualification :: Qualification
    , alias :: Alias
    } deriving Show

lookupAllowed :: ModuleName.ModuleName -> AllowedMap -> Maybe [AllowedQualification]
lookupAllowed =
  Map.lookup
