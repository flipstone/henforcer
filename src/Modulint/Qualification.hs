module Modulint.Qualification
  ( RuleMap
  , Rule(..)
  , lookupRule
  ) where

import qualified Data.Map.Strict as Map

import qualified Modulint.ModuleName as ModuleName

type RuleMap =
  Map.Map ModuleName.ModuleName Rule

data Rule
  = Forbidden
  | RequiredAs [ModuleName.ModuleName]
  | AllowedAs [ModuleName.ModuleName]
  deriving Show

lookupRule :: ModuleName.ModuleName -> RuleMap -> Maybe Rule
lookupRule =
  Map.lookup
