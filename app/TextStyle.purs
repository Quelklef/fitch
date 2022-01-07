module Fitch.TextStyle where

import Prelude
import Prelude as Prelude
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple.Nested ((/\))
import Data.String.CodePoints as String
import Data.String.CodePoints (CodePoint)
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe)

toAscii :: String -> String
toAscii =
  String.toCodePointArray
  >>> Prelude.map (\char -> Map.lookup char mapping # fromMaybe (String.singleton char))
  >>> intercalate ""

  where
    mapping :: Map CodePoint String
    mapping = Map.fromFoldable
      [ String.codePointFromChar '⊥' /\ "#"
      , String.codePointFromChar '¬' /\ "~"
      , String.codePointFromChar '∧' /\ "&"
      , String.codePointFromChar '∨' /\ "|"
      , String.codePointFromChar '→' /\ ">"
      , String.codePointFromChar '↔' /\ "<>"
      , String.codePointFromChar '∀' /\ "V"
      , String.codePointFromChar '∃' /\ "E"
      , String.codePointFromChar '≠' /\ "/="
      , String.codePointFromChar '⊢' /\ " entails "
      , String.codePointFromChar '∴' /\ "thus"
      , String.codePointFromChar '│' /\ "|"
      , String.codePointFromChar '├' /\ "+"
      , String.codePointFromChar '─' /\ "-"
      ]

map :: Boolean -> String ->  String
map useUnicode string = if useUnicode then string else toAscii string
