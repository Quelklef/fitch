module Proof exposing (..)

import Maybe exposing (Maybe)

import ListUtil
import MaybeUtil

import Formula exposing (Formula)

-- vv Something in the shape of a proof,
-- vv but containing an unknown type representing proof lines
type Proofy lineT =
  -- vv A line
  ProofLine lineT
  -- vv Assumptions and body
  | ProofBlock (List lineT) (List (Proofy lineT))

-- vv A logical proof
type alias Proof = Proofy Formula
-- vv Same structure, contains raw user strings
type alias RawProof = Proofy String

get : Int -> Proofy a -> Maybe (Proofy a)
get idx proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then ListUtil.get idx body
      else ListUtil.get (-idx-1) head
        |> Maybe.map (\formula -> ProofLine formula)

set : Int -> Proofy a -> Proofy a -> Maybe (Proofy a)
set idx subproof proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then case subproof of
        ProofLine line -> ListUtil.set idx line head
          |> Maybe.map (\newHead -> ProofBlock newHead body)
        ProofBlock _ _ -> Nothing
      else ListUtil.set (-idx-1) subproof body
        |> Maybe.map (\newBody -> ProofBlock head newBody)

remove : Int -> Proofy a -> Maybe (Proofy a)
remove idx proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then ListUtil.remove idx body |> Maybe.map (\newBody -> ProofBlock head newBody)
      else ListUtil.remove (-idx-1) head |> Maybe.map (\newHead -> ProofBlock newHead body)

firstLine : Proofy a -> Maybe a
firstLine proof = case proof of
  ProofLine line -> Just line
  ProofBlock head body ->
    List.head head
    |> MaybeUtil.orElseLazy (\() -> List.head body |> Maybe.andThen firstLine)

lastLine : Proofy a -> Maybe a
lastLine proof = case proof of
  ProofLine line -> Just line
  ProofBlock head body ->
    ListUtil.last body
    |> Maybe.andThen lastLine
    |> MaybeUtil.orElseLazy (\() -> ListUtil.last head)

assumptions : Proofy a -> List a
assumptions proof = case proof of
  ProofLine _ -> []
  ProofBlock head _ -> head

conclusion : Proofy a -> Maybe a
conclusion proof = case proof of
  ProofLine _ -> Nothing
  ProofBlock _ _ -> lastLine proof

map : (a -> b) -> Proofy a -> Proofy b
map mapper proof = case proof of
  ProofLine line -> ProofLine (mapper line)
  ProofBlock head body -> ProofBlock (List.map mapper head) (List.map (map mapper) body)

replaceM : Int -> (Proofy a -> Maybe (Proofy a)) -> Proofy a -> Maybe (Proofy a)
replaceM idx mapper proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then
        ListUtil.get idx body
        |> Maybe.andThen mapper
        |> Maybe.andThen (\newSubproof -> ListUtil.set idx newSubproof body)
        |> Maybe.map (\newBody -> ProofBlock head newBody)
      else
        ListUtil.get (-idx-1) head
        |> Maybe.map ProofLine
        |> Maybe.andThen mapper
        |> Maybe.andThen (\newSubproof -> case newSubproof of
          ProofLine newLine -> ListUtil.set (-idx-1) newLine head
          ProofBlock _ _ -> Nothing)
        |> Maybe.map (\newHead -> ProofBlock newHead body)
