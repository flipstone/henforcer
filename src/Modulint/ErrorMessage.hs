module Modulint.ErrorMessage
  ( Msg
  , Body
  , build
  , line
  , format
  , indent
  ) where

import qualified Language.Haskell.Exts.Pretty as Pretty
import qualified Language.Haskell.Exts.SrcLoc as SrcLoc

data Msg =
  Msg
    { location :: SrcLoc.SrcSpanInfo
    , body :: Body
    }

build :: SrcLoc.SrcSpanInfo -> Body -> Msg
build =
  Msg

line :: [String] -> Body
line lineWords =
  Body [lineWords]

newtype Body =
  Body [[String]]

instance Semigroup Body where
  (Body left) <> (Body right) =
    Body (left <> right)

instance Monoid Body where
  mempty =
    Body []

format :: Msg -> String
format msg =
  unlines
    ( (Pretty.prettyPrint . SrcLoc.getPointLoc . location $ msg) <> ":"
    : (formatBodyLines . indent . body $ msg)
    )

indent :: Body -> Body
indent (Body bodyLines) =
  let
    indentLine ln =
      " " : ln
  in
    Body (map indentLine bodyLines)

formatBodyLines :: Body -> [String]
formatBodyLines (Body bodyLines) =
  map unwords bodyLines

