module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Task
import Maybe exposing (Maybe)
import Html exposing (Html, Attribute, button, div, span, text, input, label, p, pre)
import Html.Attributes exposing (value, class, id, style, type_, checked)
import Html.Events exposing (onInput, keyCode, preventDefaultOn, onClick)
import Json.Decode as Json

import ListUtil
import StringUtil

import Path exposing (Path)
import Proof exposing (Proofy(..), RawProof)
import Formula exposing (Formula)
import Decorate
import Semantics
import Symbols

-- vv Modified from https://stackoverflow.com/a/41072936/4608364 and https://stackoverflow.com/a/61734163/4608364
onKeydown : ({ keyCode : Int, shiftKey : Bool } ->  { msg : Message, preventDefault : Bool }) -> Attribute Message
onKeydown respond =
    let getInfo = Json.map2 (\keyCode shiftKey -> { keyCode = keyCode, shiftKey = shiftKey })
          (Json.field "keyCode" Json.int)
          (Json.field "shiftKey" Json.bool)
        tupleRespond = respond >> (\{ msg, preventDefault } -> (msg, preventDefault))
    in preventDefaultOn "keydown" (getInfo |> Json.andThen (\info -> Json.succeed <| tupleRespond info))

-- vv Modified from https://github.com/dwyl/learn-elm/blob/master/examples/checkboxes.elm
checkbox : Bool -> msg -> String -> Html msg
checkbox isChecked msg name =
  label
    [ ]
    [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
    , text <| " " ++ name
    ]

type alias Model =
  { proof : RawProof
  , showDebugInfo : Bool
  , useUnicode : Bool
  }

main = Browser.element
  { init = init
  , subscriptions = always Sub.none
  , update = update
  , view = view
  }

init : () -> (Model, Cmd m)
init =
  let proof = ProofBlock [ "" ]
        [ ProofBlock ["-ExPx"]
          [ ProofBlock ["[a]"]
            [ ProofBlock ["Pa"]
            [ ProofLine "ExPx"
            , ProofLine "(ExPx).(-ExPx)"
            , ProofLine "#" ]
          , ProofLine "-Pa" ]
          , ProofLine "Vx-Px"
          ]
        , ProofBlock ["Vx-Px"]
            [ ProofBlock ["ExPx"]
            [ ProofBlock ["Pa", "[a]"]
              [ ProofLine "-Pa"
              , ProofLine "Pa.-Pa"
              , ProofLine "#" ]
            , ProofLine "#" ]
          , ProofLine "-ExPx" ]
        , ProofLine "(-ExPx)<->(Vx-Px)" ]
  in always ({ proof = proof, showDebugInfo = False, useUnicode = True }, Cmd.none)

type Message
  = ToggleDebugMode
  | ToggleUseUnicode
  | Noop
  | SetFocusTo Path
  | SetFormulaAt Path String
  | NewLineAfter Path Bool
  | IndentAt Path
  | DedentAt Path
  | BackspaceAt Path

setFocusTo : Path -> Cmd Message
setFocusTo path = Task.attempt (always Noop) (Dom.focus <| Path.toId path)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
  let fromDo maybeResult = case maybeResult of
        Just (newProof, cmd) -> ({ model | proof = newProof }, cmd)
        Nothing -> (model, Cmd.none)
  in case msg of
    ToggleDebugMode -> ({ model | showDebugInfo = not model.showDebugInfo }, Cmd.none)
    ToggleUseUnicode -> ({ model | useUnicode = not model.useUnicode }, Cmd.none)
    Noop -> (model, Cmd.none)
    SetFocusTo path -> (model, setFocusTo path)
    SetFormulaAt path newFormula       -> fromDo <| doSetFormulaAt path newFormula model.proof
    NewLineAfter path preferAssumption -> fromDo <| doNewLineAfter path preferAssumption model.proof
    IndentAt path                      -> fromDo <| doIndentAt path model.proof
    DedentAt path                      -> fromDo <| doDedentAt path model.proof
    BackspaceAt path                   -> fromDo <| doBackspaceAt path model.proof

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
            then Just <| ProofBlock ("" :: head) body
            else Just <| ProofBlock head (ProofLine "" :: body)

          -- vv Index targets an assumption that is not the last one
          else if idx < 0 then
            ListUtil.insert (-idx-1) "" head
            |> Maybe.map (\newHead -> ProofBlock newHead body)

          -- vv Index targets a body line
          else
            ListUtil.insert (idx + 1) (ProofLine "") body
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
      ProofLine line -> Just <| ProofBlock [line] []
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
              List.length head == 1
              -- vv The dedent is targeting the assumption
              && idx == -0-1
              -- vv The block body is empty
              && List.length body == 0
        in
          if dedentOk then ListUtil.get 0 head |> Maybe.map ProofLine
          else Nothing

    idx::idxs -> Proof.replaceM idx (\subproof -> doDedentAt_ idxs subproof) proof

doBackspaceAt : Path -> RawProof -> Maybe (RawProof, Cmd Message)
doBackspaceAt path proof =
  if not <| Path.targetsEmptyLine proof path
    then Nothing
    else
      let maybeNewProof = doBackspaceAt_ path proof
          maybeNewPath =
            if Path.targetsBody path || Path.targetsFirstAssumption proof path
            then Path.linearPred proof path
            else Just path
      in
        Maybe.map2 (\newProof newPath -> (newProof, setFocusTo newPath)) maybeNewProof maybeNewPath

doBackspaceAt_ : Path -> RawProof -> Maybe RawProof
doBackspaceAt_ path proof = case path of
  [] -> Nothing
  [idx] -> case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
            if idx < 0 then ListUtil.remove (-idx-1) head |> Maybe.map (\newHead -> ProofBlock newHead body)
            else (ListUtil.remove idx body) |> Maybe.map (\newBody -> ProofBlock head newBody)
  idx::idxs ->
    proof
    |> Proof.replaceM idx (\subproof -> doBackspaceAt_ idxs subproof)
    -- vv Don't leave behind an empty block
    |> Maybe.andThen (\newProof -> case Proof.get idx newProof of
      Just (ProofBlock head body) ->
        if List.length head == 0 && List.length body == 0
        then Proof.remove idx newProof
        else Just newProof
      _ -> Just newProof)

view : Model -> Html Message
view model =
  let proof = viewProof 0 model (Decorate.decorate model.proof)
  in
    div [ id "wrap" ]
    [ div [ id "left" ]
      [ Html.h1 [] [ text "Fitch-Style Proof Helper" ]
      , p [ class "options" ]
        [ checkbox (not model.useUnicode) ToggleUseUnicode "force plain symbols"
        , text " | "
        , checkbox model.showDebugInfo ToggleDebugMode "show debug info"
        ]
      , proof
      ]
    , div [ id "right" ]
      [ Html.h3 [] [ text "Usage" ]
      , div [] [ keyboardControls model.useUnicode ]
      ]
    ]

keyboardControls : Bool -> Html a
keyboardControls useUnicode =
  [ { keys = [".", "&", "*"], name = "conjunction (∧)" }
  , { keys = ["|",  "v", "+"], name = "disjunction (∨)" }
  , { keys = ["->"], name = "implication (→)" }
  , { keys = ["-", "~", "!"], name = "negation (¬)" }
  , { keys = ["_", "#"], name = "bottom (⊥)" }
  , { keys = ["<->"], name = "biconditional (↔)" }
  , { keys = ["\\", "V"], name = "forall (∀)" }
  , { keys = ["E", "@"], name = "exists (∃)" }
  , { keys = ["="], name = "equals (=)" }
  , { keys = ["/="], name = "does not equal (≠)" }
  , { keys = ["enter"], name = "new line" }
  , { keys = ["shift+enter"], name = "additional assumption" }
  , { keys = ["tab"], name = "up a block" }
  , { keys = ["shift+tab"], name = "down a block" }
  , { keys = ["alt+backspace"], name = "reset proof" }
  ]
  |> List.map (\{ keys, name } ->
    let keysHtml = keys |> ListUtil.flatMap (\key -> [ span [ class "key" ] [ text key ], text " " ])
    in p [] ( keysHtml ++ [ text <| Symbols.map useUnicode name ] ))
  |> div []

viewProof : Int -> Model -> Proofy Semantics.DecoratedLine -> Html Message
viewProof depth model proof = case proof of
  ProofLine { text, formula, path, lineno, justification } ->
    let isValid = justification |> Result.map (always True) |> Result.withDefault False
        isLastAssumption = Path.targetsLastAssumption model.proof path
    in div [ class <| "line" ++ StringUtil.if_ (not isValid) " --invalid" ++ StringUtil.if_ isLastAssumption " --last-assumption" ]
      [ span [ class "line:number" ] [ Html.text <| String.fromInt lineno ]
      , input
        [ class "line:input"
        , value (Formula.prettify model.useUnicode text)
        , id (Path.toId path)
        , onInput (SetFormulaAt path)
        , onKeydown (lineOnKeydown model.proof path)
        ] []
      , span [ class "line:justification" ] [ (Html.text << Symbols.map model.useUnicode) <| case justification of
          Ok justn -> justn
          Err err -> err ]
      , if model.showDebugInfo
        then let info =
                   "path: " ++ Debug.toString path ++ "\n" ++
                   "tree: " ++ Debug.toString formula
             in pre [ class "line:debug-info" ] [ Html.text info ]
        else Html.text ""
      ]

  ProofBlock head body ->
    let blockStyle =
          ListUtil.modGet depth blockColors
          |> Maybe.map (\{ backgroundColor, borderColor } -> [ style "background-color" backgroundColor, style "border-color" borderColor ])
          |> Maybe.withDefault []

    in div ([ class "block" ] ++ blockStyle) <|
      List.append
        (List.reverse (head |> List.map (ProofLine >> viewProof (depth + 1) model)))
        (body |> List.map (viewProof (depth + 1) model))

blockColors : List { borderColor: String, backgroundColor : String }
blockColors =
  [ { borderColor = "rgb(100, 100, 100)", backgroundColor = "rgb(243, 243, 243)" }
  , { borderColor = "rgb(000, 000, 200)", backgroundColor = "rgb(236, 236, 251)" }
  , { borderColor = "rgb(255, 100, 100)", backgroundColor = "rgb(255, 243, 243)" }
  , { borderColor = "rgb(120, 255, 120)", backgroundColor = "rgb(245, 255, 245)" }
  , { borderColor = "rgb(120, 050, 120)", backgroundColor = "rgb(245, 240, 245)" }
  ]

lineOnKeydown : Proofy String -> Path -> { keyCode : Int, shiftKey : Bool } -> { msg : Message, preventDefault : Bool }
lineOnKeydown wholeProof path { keyCode, shiftKey } =
  case (keyCode, shiftKey) of
    -- vv (Shift+)Enter pressed
    (13, _) ->
      let preferAssumption = shiftKey
      in { msg = NewLineAfter path preferAssumption, preventDefault = True }
    -- vv Tab pressed
    (9, False) -> { msg = IndentAt path, preventDefault = True }
    -- vv Shift+Tab pressed
    (9, True) -> { msg = DedentAt path, preventDefault = True }
    -- vv Up arrow key pressed
    (38, False) ->
      let msg = Path.linearPred wholeProof path |> Maybe.map (\newPath -> SetFocusTo newPath) |> Maybe.withDefault Noop
      in { msg = msg, preventDefault = True }
    -- vv Down arrow key pressed
    (40, False) ->
      let msg = Path.linearSucc wholeProof path |> Maybe.map (\newPath -> SetFocusTo newPath) |> Maybe.withDefault Noop
      in { msg = msg, preventDefault = True }
    -- vv Backspace key pressed
    (8, False) -> { msg = BackspaceAt path, preventDefault = False }
    -- vv Any other key pressed
    _ -> { msg = Noop, preventDefault = False }
