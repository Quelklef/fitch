module Symbols exposing (..)

import String

toAscii : String -> String
toAscii =
  String.toList
  >> List.map (\char -> case char of
    '⊥' -> "#"
    '¬' -> "~"
    '∧' -> "&"
    '∨' -> "|"
    '→' -> "->"
    '↔' -> "<->"
    '∀' -> "V"
    '∃' -> "E"
    '≠' -> "/="
    c -> String.fromChar c)
  >> String.join ""

map : Bool -> String ->  String
map useUnicode string = if useUnicode then string else toAscii string
