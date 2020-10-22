module MaybeUtil exposing (..)

import Maybe exposing (..)

orElseLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orElseLazy alternative maybe = case maybe of
  Just val -> Just val
  Nothing -> alternative ()

orElse : Maybe a -> Maybe a -> Maybe a
orElse alternative maybe = case maybe of
  Just val -> Just val
  Nothing -> alternative

fromBool : a -> Bool -> Maybe a
fromBool val bool = if bool then Just val else Nothing
