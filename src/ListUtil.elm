module ListUtil exposing (..)

import List exposing (..)

dropLast : List a -> List a
dropLast list =
  case list of
    [] -> []
    [x] -> []
    x::xs -> x :: dropLast xs
