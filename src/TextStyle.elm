module TextStyle exposing (..)

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
    '⊢' -> " entails "
    '∴' -> "thus"
    '│' -> "|"
    '├' -> "+"
    '─' -> "-"
    c -> String.fromChar c)
  >> String.join ""

map : Bool -> String ->  String
map useUnicode string = if useUnicode then string else toAscii string
