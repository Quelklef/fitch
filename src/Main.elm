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
import ArrayUtil

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
  | NewLineAfter Path Bool
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
    SetFocusTo path -> (proof, Task.attempt (always Noop) (Dom.focus <| Path.toId path))
    SetFormulaAt path newFormula -> (doSetFormulaAt path newFormula proof |> Maybe.withDefault proof, Cmd.none)
    NewLineAfter path preferAssumption -> (doNewLineAfter path preferAssumption proof |> Maybe.withDefault proof, Cmd.none)
    IndentAt path -> (doIndentAt path proof |> Maybe.withDefault proof, Cmd.none)
    DedentAt path -> (doDedentAt path proof |> Maybe.withDefault proof, Cmd.none)

doSetFormulaAt : Path -> String -> RawProof -> Maybe RawProof
doSetFormulaAt path newFormula proof =
  case path of
    [] -> case proof of
      ProofBlock _ _ -> Nothing
      ProofLine  oldFormula -> Just (ProofLine newFormula)
    idx::idxs -> Proof.replaceM idx (\subproof -> doSetFormulaAt idxs newFormula subproof) proof

-- vv If the given path targets a ProofLine, rather than a ProofBlock, then
-- vv inserts a new line after that targeted line.
-- vv If the targeted line is in the proof body, the inserted line will also
-- vv be a body line.
-- vv If the targeted line is an assumption (ie in the proof head) and is
-- vv followed by another assumption, then the inserted line will also
-- vv be an assumption.
-- vv If the targeted line is an assumption and is *not* followed by another
-- vv assumption (ie it's the last assumption), then the inserted line will
-- vv be an assumption if and only if `preferAssumption` is true; otherwise,
-- vv it will be a body line.
doNewLineAfter : Path -> Bool -> RawProof -> Maybe RawProof
doNewLineAfter path preferAssumption proof =
  case path of
    [] -> Nothing

    [idx] ->
      case proof of
        ProofLine _ -> Nothing
        ProofBlock head body ->

          -- vv Index targets last assumption
          if Path.indexTargetsLastAssumption proof idx then
            if preferAssumption
            then Just <| ProofBlock (Array.push "" head) body
            else Just <| ProofBlock head (ArrayUtil.cons (ProofLine "") body)

          -- vv Index targets an assumption that is not the last one
          else if idx < 0 then
            ArrayUtil.insert (-idx-1 + 1) "" head
            |> Maybe.map (\newHead -> ProofBlock newHead body)

          -- vv Index targets a body line
          else
            ArrayUtil.insert (idx + 1) (ProofLine "") body
            |> Maybe.map (\newBody -> ProofBlock head newBody)

    idx::idxs -> Proof.replaceM idx (\subproof -> doNewLineAfter idxs preferAssumption subproof) proof

doIndentAt : Path -> RawProof -> Maybe RawProof
doIndentAt path proof =
  case path of
    [] -> case proof of
      ProofLine line -> Just <| ProofBlock (Array.fromList [line]) Array.empty
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
              -- vv The block body is empty
              && Array.length body == 0
        in
          if dedentOk then Array.get 0 head |> Maybe.map ProofLine
          else Nothing

    idx::idxs -> Proof.replaceM idx (\subproof -> doDedentAt idxs subproof) proof


view : RawProof -> Browser.Document Message
view proof =
  let html = viewAux [] proof proof
  in
    { title = "Fitch-Stlye Proof Helper"
    , body = [html]
    }

viewAux : Path -> RawProof -> RawProof -> Html Message
viewAux path fullProof proof = case proof of
  ProofLine formula ->
    div []
      [ input
        [ value formula
        , id (Path.toId path)
        , onInput (SetFormulaAt path)
        , onKeydown (\(keyCode, shiftKey) -> case (keyCode, shiftKey) of

          -- (Shift+)Enter pressed
          (13, _) ->
            let preferAssumption = shiftKey
            in Path.linearSucc fullProof path |> Maybe.map (\pathAfter -> DoAllOf [NewLineAfter path preferAssumption, SetFocusTo pathAfter])

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
        (head |> Array.indexedMap (\idx formula -> viewAux (path ++ [-idx-1]) fullProof (ProofLine formula)))
        (body |> Array.indexedMap (\idx subproof -> viewAux (path ++ [idx]) fullProof subproof))
