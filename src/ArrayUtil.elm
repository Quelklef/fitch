module ArrayUtil exposing (..)

import Array exposing (..)

replace : Int -> (a -> a) -> Array a -> Maybe (Array a)
replace idx mapper ar =
  get idx ar
  |> Maybe.map mapper
  |> Maybe.map (\newVal -> set idx newVal ar)

strictSet : Int -> a -> Array a -> Maybe (Array a)
strictSet idx val ar = replace idx (always val) ar

appendAll : List (Array a) -> Array a
appendAll arrays = case arrays of
  [] -> empty
  ar::ars -> append ar (appendAll ars)

insert : Int -> a -> Array a -> Maybe (Array a)
insert idx val ar =
  if idx < 0 || idx > length ar then Nothing
  else Just <| appendAll
    [ slice 0 idx ar
    , fromList [val]
    , slice idx (length ar) ar
    ]

cons : a -> Array a -> Array a
cons x xs = Array.append (Array.fromList [x]) xs

last : Array a -> Maybe a
last ar = get (length ar - 1) ar
