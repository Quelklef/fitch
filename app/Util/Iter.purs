module Fitch.Util.Iter where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Data.Set as Set
import Data.Set (Set)
import Data.Tuple.Nested ((/\), type (/\))

import Fitch.Util.MaybeUtil as MaybeUtil

data Iter a
  = Fin
  | One (Unit -> a)
  | Cat (Iter a) (Unit -> Iter a)

fromArray :: forall a. Array a -> Iter a
fromArray = Array.uncons >>> case _ of
  Nothing -> Fin
  Just { head, tail } -> Cat (One $ const head) (\_ -> fromArray tail)

fromSet :: forall a. Set a -> Iter a
fromSet = Set.toUnfoldable >>> fromArray

map :: forall a b. (a -> b) -> Iter a -> Iter b
map fn = case _ of
  Fin -> Fin
  One get -> One (get >>> fn)
  Cat left right -> Cat (map fn left) (map fn <<< right)

find :: forall a. (a -> Boolean) -> Iter a -> Maybe a
find cond = case _ of
  Fin -> Nothing
  One get -> let got = get unit in MaybeUtil.fromBool got (cond got)
  Cat left right -> find cond left # MaybeUtil.orElseLazy (find cond <<< right)

flatMap :: forall a b. (a -> Iter b) -> Iter a -> Iter b
flatMap fn = case _ of
  Fin -> Fin
  One get -> Cat Fin (get >>> fn)
  Cat left right -> Cat (flatMap fn left) (flatMap fn <<< right)

filterMap :: forall a b. (a -> Maybe b) -> Iter a -> Iter b
filterMap fn = case _ of
  Fin -> Fin
  One get -> Cat Fin (\_ -> case fn (get unit) of
    Nothing -> Fin
    Just val -> One (const val))
  Cat left right -> Cat (filterMap fn left) (filterMap fn <<< right)

findMapM :: forall a b. (a -> Maybe b) -> Iter a -> Maybe b
findMapM fn = case _ of
  Fin -> Nothing
  One get -> fn (get unit)
  Cat left right -> findMapM fn left # MaybeUtil.orElseLazy (findMapM fn <<< right)

product :: forall a b. Iter a -> Iter b -> Iter (a /\ b)
product iterA iterB =
  iterA # flatMap (\a ->
  iterB # map (\b ->
    a /\ b))

product3 :: forall a b c. Iter a -> Iter b -> Iter c -> Iter (a /\ b /\ c)
product3 iterA iterB iterC =
  iterA # flatMap (\a ->
  iterB # flatMap (\b ->
  iterC # map (\c ->
    a /\ b /\ c)))
