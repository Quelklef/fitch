module Main exposing (..)

import Array exposing (Array)
import Maybe exposing (Maybe)
import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)

main = Browser.sandbox
  { init = init
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

init : RawProof
init = ProofBlock (Array.fromList ["asm"]) Array.empty

-- vv Path to a formula in a proof, as a list of indicies
-- vv A negative index is an index into a block head; positive into the body
type alias Path = List Int

proofGet : Int -> Proofy a -> Maybe (Proofy a)
proofGet idx proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx > 0 then Array.get idx body
      else Array.get (-idx-1) head
        |> Maybe.map (\formula -> ProofLine formula)

type Message =
  SetFormula Path String

update : Message -> RawProof -> RawProof
update msg proof = case msg of
  SetFormula path newFormula -> doSetFormula path newFormula proof |> Maybe.withDefault proof

doSetFormula : Path -> String -> RawProof -> Maybe RawProof
doSetFormula path newFormula proof =
  case path of
    [] -> case proof of
      ProofBlock _ _ -> Nothing
      ProofLine  oldFormula -> Just (ProofLine newFormula)
    idx::idxs ->
      proofGet idx proof
      |> Maybe.andThen (\subproof -> doSetFormula idxs newFormula subproof)

view : RawProof -> Html Message
view proof = viewAux [] proof

viewAux : Path -> RawProof -> Html Message
viewAux path proof = case proof of
  ProofLine formula ->
    div []
      [ input [ value formula, onInput (SetFormula path) ] [] ]
  ProofBlock head body ->
    div [] <|
      Array.toList <| Array.append
        (Array.indexedMap (\idx formula -> viewAux (path ++ [-idx-1]) (ProofLine formula)) head)
        (Array.indexedMap (\idx subproof -> viewAux (path ++ [idx]) subproof) body)
