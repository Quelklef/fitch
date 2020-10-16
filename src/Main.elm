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
onKeydown : ((Int, Bool) -> Message) -> Attribute Message
onKeydown respond =
    let getInfo = Json.map2 Tuple.pair
          (Json.field "keyCode" Json.int)
          (Json.field "shiftKey" Json.bool)
    in preventDefaultOn "keydown" (getInfo |> Json.andThen (\info -> Json.succeed (respond info, True)))

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
  | SetFocusTo Path
  | SetFormulaAt Path String
  | NewLineAfter Path Bool
  | IndentAt Path
  | DedentAt Path

setFocusTo : Path -> Cmd Message
setFocusTo path = Task.attempt (always Noop) (Dom.focus <| Path.toId path)

update : Message -> RawProof -> (RawProof, Cmd Message)
update msg proof =
  let result = case msg of
        Noop -> Nothing
        SetFocusTo path -> Just (proof, setFocusTo path)
        SetFormulaAt path newFormula -> doSetFormulaAt path newFormula proof
        NewLineAfter path preferAssumption -> doNewLineAfter path preferAssumption proof
        IndentAt path -> doIndentAt path proof
        DedentAt path -> doDedentAt path proof
  in result |> Maybe.withDefault (proof, Cmd.none)

doSetFormulaAt : Path -> String -> RawProof -> Maybe (RawProof, Cmd Message)
doSetFormulaAt path newFormula proof =
  doSetFormulaAt_ path newFormula proof
  |> Maybe.map (\newProof -> (newProof, Cmd.none))

doSetFormulaAt_ : Path -> String -> RawProof -> Maybe RawProof
doSetFormulaAt_ path newFormula proof =
  case path of
    [] -> case proof of
      ProofBlock _ _ -> Nothing
      ProofLine  oldFormula -> Just (ProofLine newFormula)
    idx::idxs -> Proof.replaceM idx (\subproof -> doSetFormulaAt_ idxs newFormula subproof) proof

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
doNewLineAfter : Path -> Bool -> RawProof -> Maybe (RawProof, Cmd Message)
doNewLineAfter path preferAssumption proof =
  doNewLineAfter_ path preferAssumption proof
  |> Maybe.andThen (\newProof ->
    path |> ListUtil.mapLast (\idx ->
        if Path.targetsLastAssumption newProof path then
          if preferAssumption then -1 else 0
        else if idx < 0 then idx
        else idx + 1)
    |> Maybe.map (\newPath -> (newProof, setFocusTo newPath)))

doNewLineAfter_ : Path -> Bool -> RawProof -> Maybe RawProof
doNewLineAfter_ path preferAssumption proof =
  case path of
    [] -> Nothing

    [idx] ->
      case proof of
        ProofLine _ -> Nothing
        ProofBlock head body ->

          -- vv Index targets last assumption
          if idx == -1 then
            if preferAssumption
            then Just <| ProofBlock (ArrayUtil.cons "" head) body
            else Just <| ProofBlock head (ArrayUtil.cons (ProofLine "") body)

          -- vv Index targets an assumption that is not the last one
          else if idx < 0 then
            ArrayUtil.insert (-idx-1) "" head
            |> Maybe.map (\newHead -> ProofBlock newHead body)

          -- vv Index targets a body line
          else
            ArrayUtil.insert (idx + 1) (ProofLine "") body
            |> Maybe.map (\newBody -> ProofBlock head newBody)

    idx::idxs -> Proof.replaceM idx (\subproof -> doNewLineAfter_ idxs preferAssumption subproof) proof

doIndentAt : Path -> RawProof -> Maybe (RawProof, Cmd Message)
doIndentAt path proof =
  doIndentAt_ path proof
  |> Maybe.map (\newProof -> (newProof, setFocusTo <| path ++ [-0-1]))

doIndentAt_ : Path -> RawProof -> Maybe RawProof
doIndentAt_ path proof =
  case path of
    [] -> case proof of
      ProofLine line -> Just <| ProofBlock (Array.fromList [line]) Array.empty
      ProofBlock _ _ -> Nothing
    idx::idxs -> Proof.replaceM idx (\subproof -> doIndentAt_ idxs subproof) proof

doDedentAt : Path -> RawProof -> Maybe (RawProof, Cmd Message)
doDedentAt path proof =
  doDedentAt_ path proof
  |> Maybe.map (\newProof -> (newProof, setFocusTo <| ListUtil.dropLast path))

doDedentAt_ : Path -> RawProof -> Maybe RawProof
doDedentAt_ path proof =
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

    idx::idxs -> Proof.replaceM idx (\subproof -> doDedentAt_ idxs subproof) proof


view : RawProof -> Browser.Document Message
view proof =
  let html = view_ [] proof proof
  in
    { title = "Fitch-Stlye Proof Helper"
    , body = [html]
    }

view_ : Path -> RawProof -> RawProof -> Html Message
view_ path fullProof proof = case proof of
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
            in NewLineAfter path preferAssumption

          -- Tab pressed
          (9, False) -> IndentAt path

          -- Shift+Tab pressed
          (9, True) -> DedentAt path

          -- Up arrow key pressed
          (38, False) -> Path.linearPred fullProof path |> Maybe.map (\newPath -> SetFocusTo newPath) |> Maybe.withDefault Noop

          -- Down arrow key pressed
          (40, False) -> Path.linearSucc fullProof path |> Maybe.map (\newPath -> SetFocusTo newPath) |> Maybe.withDefault Noop

          -- Any other key pressed
          _ -> Noop

        )
        ] []
      , text (Debug.toString path)
      ]

  ProofBlock head body ->
    div [ style "margin-left" "20px" ] <|
      Array.toList <| Array.append
        (ArrayUtil.reverse (head |> Array.indexedMap (\idx formula -> view_ (path ++ [-idx-1]) fullProof (ProofLine formula))))
        (body |> Array.indexedMap (\idx subproof -> view_ (path ++ [idx]) fullProof subproof))
