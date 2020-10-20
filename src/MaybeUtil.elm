module MaybeUtil exposing (..)

import Maybe exposing (..)

orLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orLazy alternative maybe = case maybe of
  Just val -> Just val
  Nothing -> alternative ()

fromBool : a -> Bool -> Maybe a
fromBool val bool = if bool then Just val else Nothing
