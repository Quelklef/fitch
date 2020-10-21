module Symbol exposing (..)

import String

toAscii : String -> String
toAscii =
  String.map (\char -> case char of
    "⊥" -> "#"
    "¬" -> "~"
    "∧" -> "&"
    "∨" -> "|"
    "→" -> "->"
    "↔" -> "<->"
    "∀" -> "V"
    "∃" -> "E"
    "≠" -> "/="
    c -> c)
