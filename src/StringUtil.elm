module StringUtil exposing (..)

import String exposing (..)

-- why is this not built-in
string_fromBool : Bool -> String
string_fromBool bool = case bool of
  True -> "True"
  False -> "False"
