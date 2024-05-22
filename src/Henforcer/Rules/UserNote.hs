{- |
Module      : Henforcer.Rules.UserNote
Description :
Copyright   : (c) Flipstone Technology Partners, 2024
License     : BSD-3-clause
Maintainer  : maintainers@flipstone.com
-}
module Henforcer.Rules.UserNote
  ( UserNote (..)
  , userNoteField
  , FailureWithUserNote (userNotes, underlyingFailure)
  , failureWithUserNotes
  , failureWithUserNote
  , failureWithNoNote
  , noUserNote
  ) where

import qualified Data.Maybe as Maybe
import qualified Toml

import qualified CompatGHC
import qualified TomlHelper

newtype UserNote = UserNote (Maybe String)

userNoteMbStr :: UserNote -> Maybe String
userNoteMbStr (UserNote mbStr) = mbStr

userNoteCodec :: Toml.Key -> Toml.TomlCodec UserNote
userNoteCodec =
  Toml.diwrap . Toml.dioptional . Toml.string

userNoteField :: (object -> UserNote) -> Toml.Codec object UserNote
userNoteField accessor =
  TomlHelper.addField "note" accessor userNoteCodec

noUserNote :: UserNote
noUserNote = UserNote Nothing

data FailureWithUserNote a = FailureWithUserNote
  { userNotes :: ![UserNote]
  , underlyingFailure :: !a
  }

instance (CompatGHC.Outputable a) => CompatGHC.Outputable (FailureWithUserNote a) where
  ppr withUserNote =
    let
      underlyingDoc = CompatGHC.ppr (underlyingFailure withUserNote)
     in
      case Maybe.mapMaybe userNoteMbStr $ userNotes withUserNote of
        [] -> underlyingDoc
        noteStrs ->
          CompatGHC.vcat $ pure underlyingDoc <> fmap (CompatGHC.text . ("Note: " <>)) noteStrs

failureWithUserNotes :: [UserNote] -> a -> FailureWithUserNote a
failureWithUserNotes = FailureWithUserNote

failureWithUserNote :: UserNote -> a -> FailureWithUserNote a
failureWithUserNote note = failureWithUserNotes (pure note)

failureWithNoNote :: a -> FailureWithUserNote a
failureWithNoNote = failureWithUserNote noUserNote
