{-# LANGUAGE ImportQualifiedPost #-}
module Service.ThirdPartyPetsSite.Implementation where

-- This import is not allowed because it is a backward dependency from
-- 'Service' to 'PetStore'
import qualified PetStore.Pet.Model as Model
import Data.List qualified as List
import Data.Bool
