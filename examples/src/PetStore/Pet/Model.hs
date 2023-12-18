{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : PetStore.Pet.Model
Description : An example module
Copyright   : (c) Flipstone, 2022
License     : BSD-3-clause
Maintainer  : sample@email.com

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module PetStore.Pet.Model where

import Control.Monad as M
import Data.Foldable as F
import Data.Functor as F
import Data.Monoid as M
import Data.Text
import Prelude qualified
import System.IO.Unsafe (unsafePerformIO)
import qualified "unliftio" UnliftIO as Foo
import qualified UnliftIO.Async as Foo

-- Note this is not haddock
bar :: Prelude.IO ()
bar = Prelude.undefined

{-| This is a
longer
form
haddock

@since 6.7.8.9
-}
baz = Prelude.undefined

-- | This is some haddock
foo :: Prelude.IO ()
foo = unsafePerformIO Prelude.undefined

data Bar =
  A_BAR -- ^ Thing
  | B_BAR -- ^ B

class MyShow a where
  myShow :: a -> Prelude.String
