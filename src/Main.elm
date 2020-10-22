module Main exposing (..)

import Browser
import Browser.Dom as Dom

import Types exposing (Model, Proofy(..))
import Update exposing (update)
import View exposing (view)

main = Browser.element
  { init = init
  , subscriptions = always Sub.none
  , update = update
  , view = view
  }

init : () -> (Model, Cmd m)
init = always ({ proof = ProofBlock [""] [], showDebugInfo = False, useUnicode = True }, Cmd.none)
