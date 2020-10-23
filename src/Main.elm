module Main exposing (..)

import Browser
import Browser.Dom as Dom

import Types exposing (Model, Proofy(..))
import Update exposing (update)
import View exposing (view)
import Serialize

main = Browser.element
  { init = init
  , subscriptions = always Sub.none
  , update = update
  , view = view
  }

init : String -> (Model, Cmd m)
init proofFromUrl =
  proofFromUrl
  |> Serialize.deserialize
  |> Maybe.withDefault (ProofBlock [""] [])
  |> (\proof -> ({ proof = proof, showDebugInfo = False, useUnicode = True }, Cmd.none))
