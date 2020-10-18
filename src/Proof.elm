module Proof exposing (..)

import Array exposing (Array)
import Maybe exposing (Maybe)

import ArrayUtil

import Formula exposing (Formula)

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

get : Int -> Proofy a -> Maybe (Proofy a)
get idx proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then Array.get idx body
      else Array.get (-idx-1) head
        |> Maybe.map (\formula -> ProofLine formula)

set : Int -> Proofy a -> Proofy a -> Maybe (Proofy a)
set idx subproof proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then case subproof of
        ProofLine line -> ArrayUtil.strictSet idx line head
          |> Maybe.map (\newHead -> ProofBlock newHead body)
        ProofBlock _ _ -> Nothing
      else ArrayUtil.strictSet (-idx-1) subproof body
        |> Maybe.map (\newBody -> ProofBlock head newBody)

remove : Int -> Proofy a -> Maybe (Proofy a)
remove idx proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then ArrayUtil.remove idx body |> Maybe.map (\newBody -> ProofBlock head newBody)
      else ArrayUtil.remove (-idx-1) head |> Maybe.map (\newHead -> ProofBlock newHead body)

replaceM : Int -> (Proofy a -> Maybe (Proofy a)) -> Proofy a -> Maybe (Proofy a)
replaceM idx mapper proof =
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
