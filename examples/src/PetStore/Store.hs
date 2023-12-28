{-|
Module      : W
Description : Short description
Copyright   : (c) Some Person, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module PetStore.Store
  (
  add1
  ) where

import Data.Bool
import Data.List

-- | Note this is haddock
-- Some documentation
--
-- @since 0.1
add1 :: Integer -> Integer
add1 = (+) 1
