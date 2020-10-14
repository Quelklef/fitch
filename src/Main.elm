module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Task
import Array exposing (Array)
import Maybe exposing (Maybe)
import Html exposing (Html, Attribute, button, div, text, input)
import Html.Attributes exposing (value, style, id)
import Html.Events exposing (onInput, keyCode, preventDefaultOn)
import Json.Decode as Json

import ListUtil

import Path exposing (Path)
import Proof exposing (Proofy(..), RawProof)

-- vv Modified from https://stackoverflow.com/a/41072936/4608364 and https://stackoverflow.com/a/61734163/4608364
onKeydown : ((Int, Bool) -> Maybe Message) -> Attribute Message
onKeydown respond =
    let
      getInfo = Json.map2 Tuple.pair
          (Json.field "keyCode" Json.int)
          (Json.field "shiftKey" Json.bool)
      jsonRespond info =
          case respond info of
            Nothing -> Json.fail "Maybe.Nothing"
            Just msg -> Json.succeed (msg, True)
    in preventDefaultOn "keydown" (Json.andThen jsonRespond getInfo)

main = Browser.document
  { init = init
  , subscriptions = always Sub.none
  , update = update
  , view = view
  }

init : () -> (RawProof, Cmd m)
init =
  let proof = ProofBlock (Array.fromList ["assumption"]) (Array.fromList [ProofLine "consequence"])
  in always (proof, Cmd.none)

type Message =
  Noop
  | DoAllOf (List Message)
  | SetFocusTo Path
  | SetFormulaAt Path String
  | AppendLineAfter Path
  | IndentAt Path
  | DedentAt Path

update : Message -> RawProof -> (RawProof, Cmd Message)
update msg proof =
  case msg of
    Noop -> (proof, Cmd.none)
    DoAllOf [] -> (proof, Cmd.none)
    DoAllOf (msgHead::msgRest) ->
      let (updatedProof1, cmd1) = update msgHead proof
          (updatedProof2, cmd2) = update (DoAllOf msgRest) updatedProof1
      in (updatedProof2, Cmd.batch [cmd1, cmd2])
    SetFocusTo path            -> (proof, Task.attempt (always Noop) (Dom.focus <| Path.toId path))
    SetFormulaAt path newFormula -> (doSetFormulaAt path newFormula proof |> Maybe.withDefault proof, Cmd.none)
    AppendLineAfter path       -> (doAppendLineAfter path proof       |> Maybe.withDefault proof, Cmd.none)
    IndentAt path              -> (doIndentAt path proof              |> Maybe.withDefault proof, Cmd.none)
    DedentAt path              -> (doDedentAt path proof              |> Maybe.withDefault proof, Cmd.none)

doSetFormulaAt : Path -> String -> RawProof -> Maybe RawProof
doSetFormulaAt path newFormula proof =
  case path of
    [] -> case proof of
      ProofBlock _ _ -> Nothing
      ProofLine  oldFormula -> Just (ProofLine newFormula)
    idx::idxs -> Proof.replaceM idx (\subproof -> doSetFormulaAt idxs newFormula subproof) proof

doAppendLineAfter : Path -> RawProof -> Maybe RawProof
doAppendLineAfter path proof =
  case path of
    [] -> Nothing
    [idx] -> Proof.insertLineAfter idx "" proof
    idx::idxs -> Proof.replaceM idx (\subproof -> doAppendLineAfter idxs subproof) proof

doIndentAt : Path -> RawProof -> Maybe RawProof
doIndentAt path proof =
  case path of
    [] -> case proof of
      ProofLine line -> Just <| ProofBlock (Array.fromList [line]) (Array.fromList [ProofLine ""])
      ProofBlock _ _ -> Nothing
    idx::idxs -> Proof.replaceM idx (\subproof -> doIndentAt idxs subproof) proof

doDedentAt : Path -> RawProof -> Maybe RawProof
doDedentAt path proof =
  case path of
    [] -> Nothing

    [idx] -> case proof of
      ProofLine _ -> Nothing
      ProofBlock head body ->
        -- We allow dedenting a block back to a line only if:
        let dedentOk =
              -- vv The block has only one assumption
              Array.length head == 1
              -- vv The dedent is targeting the assumption
              && idx == -0-1
              -- vv The block has only one subproof in its body
              && Array.length body == 1
              -- vv That subproof is an empty line
              && (Array.get 0 body
                |> Maybe.map (\subproof -> case subproof of
                  ProofLine line -> line == ""
                  ProofBlock _ _ -> False)
                |> Maybe.withDefault False)
        in
          if dedentOk then Array.get 0 head |> Maybe.map ProofLine
          else Nothing

    idx::idxs -> Proof.replaceM idx (\subproof -> doDedentAt idxs subproof) proof


view : RawProof -> Browser.Document Message
view proof =
  let html = viewAux [] proof
  in
    { title = "Fitch-Stlye Proof Helper"
    , body = [html]
    }

viewAux : Path -> RawProof -> Html Message
viewAux path proof = case proof of
  ProofLine formula ->
    div []
      [ input
        [ value formula
        , id (Path.toId path)
        , onInput (SetFormulaAt path)
        , onKeydown (\(keyCode, shiftKey) -> case (keyCode, shiftKey) of

          -- Enter pressed
          (13, False) -> Path.next path |> Maybe.map (\pathAfter -> DoAllOf [AppendLineAfter path, SetFocusTo pathAfter])

          -- Tab pressed
          (9, False) -> Just <| DoAllOf [IndentAt path, SetFocusTo (path ++ [-0-1])]

          -- Shift+Tab pressed
          (9, True) -> Just <| DoAllOf [DedentAt path, SetFocusTo (ListUtil.dropLast path)]

          -- Any other key pressed
          _ -> Nothing

        )
        ] []
      , text (Debug.toString path)
      ]

  ProofBlock head body ->
    div [ style "margin-left" "20px" ] <|
      Array.toList <| Array.append
        (Array.indexedMap (\idx formula -> viewAux (path ++ [-idx-1]) (ProofLine formula)) head)
        (Array.indexedMap (\idx subproof -> viewAux (path ++ [idx]) subproof) body)
