module Fitch.Util.ArrayUtil where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))

import Fitch.Util.MaybeUtil as MaybeUtil

get :: forall a. Int -> Array a -> Maybe a
get = flip Array.index

set :: forall a. Int -> a -> Array a -> Maybe (Array a)
set = Array.updateAt

remove :: forall a. Int -> Array a -> Maybe (Array a)
remove = Array.deleteAt

insert :: forall a. Int -> a -> Array a -> Maybe (Array a)
insert = Array.insertAt

last :: forall a. Array a -> Maybe a
last ar = get (Array.length ar - 1) ar

dropLast :: forall a. Array a -> Array a
dropLast ar = Array.slice 0 (Array.length ar - 1) ar

mapLast :: forall a. (a -> a) -> Array a -> Maybe (Array a)
mapLast fn ar = Array.alterAt (Array.length ar - 1) (Just <<< fn) ar

startsWith :: forall a. Eq a => Array a -> Array a -> Boolean
startsWith prefix ar = Array.slice 0 (Array.length prefix) ar == prefix

findMapM :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
findMapM fn = Array.uncons >>> case _ of
  Nothing -> Nothing
  Just { head, tail } -> fn head # MaybeUtil.orElseLazy (\_ -> findMapM fn tail)

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust = Array.uncons >>> case _ of
  Nothing -> Nothing
  Just { head: Just val } -> Just val
  Just { tail } -> firstJust tail

find :: forall a. (a -> Boolean) -> Array a -> Maybe a
find cond = Array.uncons >>> case _ of
  Nothing -> Nothing
  Just { head, tail } -> if cond head then Just head else find cond tail

modGet :: forall a. Int -> Array a -> Maybe a
modGet idx = case _ of
  [] -> Nothing
  ar -> ar # get (idx `mod` Array.length ar)

flatMap :: forall a b. (a -> Array b) -> Array a -> Array b
flatMap fn = Array.uncons >>> case _ of
  Nothing -> []
  Just { head, tail } -> fn head <> flatMap fn tail

push :: forall a. a -> Array a -> Array a
push x xs = xs <> [x]

init :: forall a. Array a -> Array a
init ar = Array.slice 0 (Array.length ar - 1) ar

zip :: forall a b. Array a -> Array b -> Array (a /\ b)
zip xs0 ys0 = case Array.uncons xs0, Array.uncons ys0 of
  Just { head: x, tail: xs }, Just { head: y, tail: ys } -> Array.cons (x /\ y) $ zip xs ys
  _, _ -> []

if_ :: forall a. Boolean -> a -> Array a
if_ cond v = if cond then [v] else []
