module Iter exposing (..)

import Set exposing (Set)

import MaybeUtil

type Iter a =
  Fin
  | One (() -> a)
  | Cat (Iter a) (() -> Iter a)

fromList : List a -> Iter a
fromList list = case list of
  [] -> Fin
  x::xs -> Cat (One (always x)) (\() -> fromList xs)

fromSet : Set a -> Iter a
fromSet = Set.toList >> fromList

map : (a -> b) -> Iter a -> Iter b
map mapper iter = case iter of
  Fin -> Fin
  One get -> One (get >> mapper)
  Cat left right -> Cat (map mapper left) (map mapper << right)

find : (a -> Bool) -> Iter a -> Maybe a
find cond iter = case iter of
  Fin -> Nothing
  One get -> let got = get () in MaybeUtil.fromBool got (cond got)
  Cat left right -> find cond left |> MaybeUtil.orElseLazy (find cond << right)

flatMap : (a -> Iter b) -> Iter a -> Iter b
flatMap mapper iter = case iter of
  Fin -> Fin
  One get -> Cat Fin (get >> mapper)
  Cat left right -> Cat (flatMap mapper left) (flatMap mapper << right)

filterMap : (a -> Maybe b) -> Iter a -> Iter b
filterMap mapper iter = case iter of
  Fin -> Fin
  One get -> Cat Fin (\() -> case mapper (get ()) of
    Nothing -> Fin
    Just val -> One (always val))
  Cat left right -> Cat (filterMap mapper left) (filterMap mapper << right)

findMapM : (a -> Maybe b) -> Iter a -> Maybe b
findMapM mapper iter = case iter of
  Fin -> Nothing
  One get -> mapper (get ())
  Cat left right -> findMapM mapper left |> MaybeUtil.orElseLazy (findMapM mapper << right)

product : Iter a -> Iter b -> Iter (a, b)
product iterA iterB =
  iterA |> flatMap (\a ->
  iterB |> map (\b ->
    (a, b)))

product3 : Iter a -> Iter b -> Iter c -> Iter (a, b, c)
product3 iterA iterB iterC =
  iterA |> flatMap (\a ->
  iterB |> flatMap (\b ->
  iterC |> map (\c ->
    (a, b, c))))
