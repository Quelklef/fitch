module Iter exposing (..)

import MaybeUtil

type Iter a =
  Fin
  | One (() -> a)
  | Cat (Iter a) (() -> Iter a)

fromList : List a -> Iter a
fromList list = case list of
  [] -> Fin
  x::xs -> Cat (One (always x)) (\() -> fromList xs)

map : (a -> b) -> Iter a -> Iter b
map mapper iter = case iter of
  Fin -> Fin
  One get -> One (get >> mapper)
  Cat left right -> Cat (map mapper left) (map mapper << right)

flatMap : (a -> Iter b) -> Iter a -> Iter b
flatMap mapper iter = case iter of
  Fin -> Fin
  One get -> Cat Fin (get >> mapper)
  Cat left right -> Cat (flatMap mapper left) (flatMap mapper << right)

product3 : Iter a -> Iter b -> Iter c -> Iter (a, b, c)
product3 iterA iterB iterC =
  iterA |> flatMap (\a ->
  iterB |> flatMap (\b ->
  iterC |> map (\c ->
    (a, b, c))))

findMapM : (a -> Maybe b) -> Iter a -> Maybe b
findMapM mapper iter = case iter of
  Fin -> Nothing
  One get -> mapper <| get ()
  Cat left right -> findMapM mapper left |> MaybeUtil.orLazy (findMapM mapper << right)
