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

array_replace : Int -> (a -> a) -> Array a -> Maybe (Array a)
array_replace idx mapper ar =
  Array.get idx ar
  |> Maybe.map mapper
  |> Maybe.map (\newVal -> Array.set idx newVal ar)

array_strictSet : Int -> a -> Array a -> Maybe (Array a)
array_strictSet idx val ar = array_replace idx (always val) ar

array_appendAll : List (Array a) -> Array a
array_appendAll arrays = case arrays of
  [] -> Array.empty
  ar::ars -> Array.append ar (array_appendAll ars)

array_insert : Int -> a -> Array a -> Maybe (Array a)
array_insert idx val ar =
  if idx < 0 || idx > Array.length ar then Nothing
  else Just <| array_appendAll
    [ Array.slice 0 idx ar
    , Array.fromList [val]
    , Array.slice idx (Array.length ar) ar
    ]

-- why is this not built-in
string_fromBool : Bool -> String
string_fromBool bool = case bool of
  True -> "True"
  False -> "False"

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

type Formula =
  Empty
  | Variable String
  | Negation Formula
  | Conjunction Formula
  | Disjunction Formula
  | Implication Formula
  | Forall String Formula
  | Exists String Formula
  | Declaring String Formula

-- vv Something in the shape of a proof,
-- vv but containing an unknown type representing proof lines
type Proofy lineT =
  -- vv A line
  ProofLine lineT
  -- vv Assumptions and body
  | ProofBlock (Array lineT) (Array (Proofy lineT))

-- vv A logical proof
type alias Proof = Proofy Formula
-- vv Same structure, contains raw user strings
type alias RawProof = Proofy String

init : () -> (RawProof, Cmd m)
init =
  let proof = ProofBlock (Array.fromList ["assumption"]) (Array.fromList [ProofLine "consequence"])
  in always (proof, Cmd.none)

-- vv Path to a formula in a proof, as a list of indicies
-- vv A negative index is an index into a block head; positive into the body
type alias Path = List Int

pathNext : Path -> Maybe Path
pathNext path = case path of
  [] -> Nothing
  [idx] ->
    let newIdx = if idx >= 0 then idx + 1 else idx - 1
    in Just [newIdx]
  idx::idxs -> pathNext idxs |> Maybe.map (\tail -> idx :: tail)

pathToId : Path -> String
pathToId path = "path_" ++ String.join "_" (List.map String.fromInt path)

proofGet : Int -> Proofy a -> Maybe (Proofy a)
proofGet idx proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then Array.get idx body
      else Array.get (-idx-1) head
        |> Maybe.map (\formula -> ProofLine formula)

proofSet : Int -> Proofy a -> Proofy a -> Maybe (Proofy a)
proofSet idx subproof proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then case subproof of
        ProofLine line -> array_strictSet idx line head
          |> Maybe.map (\newHead -> ProofBlock newHead body)
        ProofBlock _ _ -> Nothing
      else array_strictSet (-idx-1) subproof body
        |> Maybe.map (\newBody -> ProofBlock head newBody)

proofReplaceM : Int -> (Proofy a -> Maybe (Proofy a)) -> Proofy a -> Maybe (Proofy a)
proofReplaceM idx mapper proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then
        Array.get idx body
        |> Maybe.andThen mapper
        |> Maybe.map (\newSubproof -> Array.set idx newSubproof body)
        |> Maybe.map (\newBody -> ProofBlock head newBody)
      else
        Array.get (-idx-1) head
        |> Maybe.map ProofLine
        |> Maybe.andThen mapper
        |> Maybe.andThen (\newSubproof -> case newSubproof of
          ProofLine newLine -> Just <| Array.set (-idx-1) newLine head
          ProofBlock _ _ -> Nothing)
        |> Maybe.map (\newHead -> ProofBlock newHead body)

proofInsertLineAfter : Int -> a -> Proofy a -> Maybe (Proofy a)
proofInsertLineAfter idx line proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then array_insert (idx + 1) (ProofLine line) body
        |> Maybe.map (\newBody -> ProofBlock head newBody)
      else array_insert (-idx-1 + 1) line head
        |> Maybe.map (\newHead -> ProofBlock newHead body)

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
    SetFocusTo path            -> (proof, Task.attempt (always Noop) (Dom.focus <| pathToId path))
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
    idx::idxs -> proofReplaceM idx (\subproof -> doSetFormulaAt idxs newFormula subproof) proof

doAppendLineAfter : Path -> RawProof -> Maybe RawProof
doAppendLineAfter path proof =
  case path of
    [] -> Nothing
    [idx] -> proofInsertLineAfter idx "" proof
    idx::idxs -> proofReplaceM idx (\subproof -> doAppendLineAfter idxs subproof) proof

doIndentAt : Path -> RawProof -> Maybe RawProof
doIndentAt path proof =
  case path of
    [] -> case proof of
      ProofLine line -> Just <| ProofBlock (Array.fromList [line]) (Array.fromList [ProofLine ""])
      ProofBlock _ _ -> Nothing
    idx::idxs -> proofReplaceM idx (\subproof -> doIndentAt idxs subproof) proof

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

    idx::idxs -> proofReplaceM idx (\subproof -> doDedentAt idxs subproof) proof


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
        , id (pathToId path)
        , onInput (SetFormulaAt path)
        , onKeydown (\(keyCode, shiftKey) -> case (keyCode, shiftKey) of

          -- Enter pressed
          (13, False) -> pathNext path |> Maybe.map (\pathAfter -> DoAllOf [AppendLineAfter path, SetFocusTo pathAfter])

          -- Tab pressed
          (9, False) -> Just <| IndentAt path

          -- Shift+Tab pressed
          (9, True) -> Just <| DedentAt path

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
