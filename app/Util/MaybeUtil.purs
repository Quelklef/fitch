module Fitch.Util.MaybeUtil where

import Prelude
import Data.Maybe (Maybe (..))

orElseLazy :: forall a. (Unit -> Maybe a) -> Maybe a -> Maybe a
orElseLazy alt = case _ of
  Just x -> Just x
  Nothing -> alt unit
