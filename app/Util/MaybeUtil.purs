module Fitch.Util.MaybeUtil where

import Prelude
import Data.Maybe (Maybe (..))

orElseLazy :: forall a. (Unit -> Maybe a) -> Maybe a -> Maybe a
orElseLazy alt = case _ of
  Just x -> Just x
  Nothing -> alt unit

fromBool :: forall a. a -> Boolean -> Maybe a
fromBool val = case _ of
  true -> Just val
  false -> Nothing
