module ListUtil exposing (..)

import List exposing (..)

import MaybeUtil

get : Int -> List a -> Maybe a
get idx list = case list of
  [] -> Nothing
  head::tail ->
    if idx == 0
    then Just head
    else get (idx - 1) tail

set : Int -> a -> List a -> Maybe (List a)
set idx val list = case list of
  [] -> Nothing
  head::tail ->
    if idx == 0
    then Just (val::tail)
    else set (idx - 1) val tail
         |> Maybe.map (\newTail -> head :: newTail)

remove : Int -> List a -> Maybe (List a)
remove idx list = case list of
  [] -> Nothing
  head::tail ->
    if idx == 0 then Just tail
    else remove (idx - 1) tail
         |> Maybe.map (\newTail -> head :: newTail)

insert : Int -> a -> List a -> Maybe (List a)
insert idx val list =
  if idx == 0
  then Just (val::list)
  else case list of
    [] -> Nothing
    head::tail ->
      insert (idx - 1) val tail
      |> Maybe.map (\newTail -> head :: newTail)

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

startsWith : List a -> List a -> Bool
startsWith prefix list = case prefix of
  [] -> True
  p::ps ->
    if head list == Just p
    then startsWith ps (drop 1 list)
    else False

findMapM : (a -> Maybe b) -> List a -> Maybe b
findMapM mapper items = case items of
  [] -> Nothing
  x::xs -> mapper x |> MaybeUtil.orElseLazy (\() -> findMapM mapper xs)

find : (a -> Bool) -> List a -> Maybe a
find cond items = case items of
  [] -> Nothing
  head::tail -> if cond head then Just head else find cond tail

modGet : Int -> List a -> Maybe a
modGet idx list =
  if List.length list == 0
  then Nothing
  else list |> get (modBy (List.length list) idx)

flatMap : (a -> List b) -> List a -> List b
flatMap mapper list = case list of
  [] -> []
  head::tail -> mapper head ++ flatMap mapper tail

push : a -> List a -> List a
push x xs = xs ++ [x]

init : List a -> List a
init list = case list of
  [] -> []
  [a, b] -> [a]
  x::xs -> x :: init xs

zip : List a -> List b -> List (a, b)
zip xs ys = case (xs, ys) of
  (x::xtail, y::ytail) -> (x, y) :: zip xtail ytail
  _ -> []

if_ : Bool -> a -> List a
if_ cond val = if cond then [val] else []
