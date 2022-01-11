module Fitch.Util.ArrayUtil where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Array as Array

startsWith :: forall a. Eq a => Array a -> Array a -> Boolean
startsWith prefix ar = Array.slice 0 (Array.length prefix) ar == prefix

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust = Array.uncons >>> case _ of
  Nothing -> Nothing
  Just { head: Just val } -> Just val
  Just { tail } -> firstJust tail

modGet :: forall a. Int -> Array a -> Maybe a
modGet idx = case _ of
  [] -> Nothing
  ar -> Array.index ar (idx `mod` Array.length ar)
