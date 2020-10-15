module ListUtil exposing (..)

import List exposing (..)

last : List a -> Maybe a
last list = case list of
  [] -> Nothing
  [x] -> Just x
  x::xs -> last xs

dropLast : List a -> List a
dropLast list =
  case list of
    [] -> []
    [x] -> []
    x::xs -> x :: dropLast xs

mapLast : (a -> a) -> List a -> Maybe (List a)
mapLast mapper list =
  case list of
    [] -> Nothing
    [x] -> Just [mapper x]
    x::xs -> mapLast mapper xs |> Maybe.map (\newList -> x :: newList)
