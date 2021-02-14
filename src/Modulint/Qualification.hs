module Modulint.Qualification
  ( AllowedSchemes
  , Qualification(..)
  , Alias(..)
  , Scheme(..)
  , lookupAllowedSchemes
  ) where

import qualified Data.Map.Strict as Map

import qualified Modulint.ModuleName as ModuleName

type AllowedSchemes =
  Map.Map ModuleName.ModuleName [Scheme]

data Qualification
  = Qualified
  | Unqualified
  deriving (Show, Eq, Ord)

data Alias
  = WithAlias ModuleName.ModuleName
  | WithoutAlias
  deriving (Show, Eq, Ord)

data Scheme =
  Scheme
    { qualification :: Qualification
    , alias :: Alias
    } deriving (Show, Eq, Ord)

lookupAllowedSchemes :: ModuleName.ModuleName -> AllowedSchemes -> Maybe [Scheme]
lookupAllowedSchemes =
  Map.lookup
