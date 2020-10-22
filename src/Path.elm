module Path exposing (..)

import List
import Maybe exposing (Maybe)

import ListUtil

import Types exposing (Proofy(..), Path)
import Proof

pathToLastLine : Proofy a -> Maybe Path
pathToLastLine proof = case proof of
  ProofLine _ -> Just []
  ProofBlock head body ->
    let maybeIdx =
          if List.length body > 0 then Just <| List.length body - 1
          else if List.length head > 0 then Just <| -1
          else Nothing
    in maybeIdx
      |> Maybe.andThen (\idx -> Proof.get idx proof
      |> Maybe.andThen (\subproof -> pathToLastLine subproof)
      |> Maybe.map (\pathTail -> idx :: pathTail))

pathToFirstLine : Proofy a -> Maybe Path
pathToFirstLine proof = case proof of
  ProofLine _ -> Just []
  ProofBlock head body ->
    let maybeIdx =
          if List.length head > 0 then Just -(List.length head)
          else if List.length body > 0 then Just 0
          else Nothing
    in maybeIdx
      |> Maybe.andThen (\idx -> Proof.get idx proof
      |> Maybe.andThen (\subproof -> pathToFirstLine subproof)
      |> Maybe.map (\pathTail -> idx :: pathTail))

targetsLine : Proofy a -> Int -> Bool
targetsLine proof idx =
  case Proof.get idx proof of
    Just (ProofLine _) -> True
    _ -> False

-- vv Given an index targeting a line, does the following:
-- vv Given a path in a proof that targets a line, evaluates the
-- vv path which targets the "linearly next" line, ignoring nesting.
-- vv For instance, consider the following:
-- vv   ProofBlock
-- vv     [ "line 1"
-- vv     , "line 2" ]
-- vv     [ ProofLine "line 3"
-- vv     , ProofBlock
-- vv         [ "line 4" ]
-- vv         [ ProofLine "line 5" ]
-- vv     , ProofLine "line 6" ]
-- vv The above example names the lines as per their linear order.
-- vv Thus the linear successor of the path [-1], which targets "line 1",
-- vv is [-2], which targets "line 2"; the linear successor of that
-- vv is [0], targeting "line 3", then [1, -1], targeting "line 4",
-- vv then [1, 0], targeting "line 5", and finally [2], targeting "line 6"
linearSucc : Proofy a -> Path -> Maybe Path
linearSucc proof path = case path of
  [] -> Nothing
  idx::idxs ->
    let
        -- vv Attempt to look for the result at the current idx
        hereAttempt =
          Proof.get idx proof
          |> Maybe.andThen (\subproof -> linearSucc subproof idxs)
          |> Maybe.map (\pathTail -> idx :: pathTail)

        -- vv If that fails, we'll look for the result in the succeeding index
        succAttempt () =  -- make it a function so that it's lazy
          Proof.get (idx + 1) proof
            |> Maybe.andThen (\subproof -> case subproof of
              ProofLine _ -> Just [idx + 1]
              ProofBlock _ _ -> pathToFirstLine subproof |> Maybe.map (\pathTail -> (idx + 1) :: pathTail))

    in case hereAttempt of
      Just _ -> hereAttempt
      Nothing -> succAttempt ()

-- vv Like linearSucc, but in the opposite direction
linearPred : Proofy a -> Path -> Maybe Path
linearPred proof path = case path of
  [] -> Nothing
  idx::idxs ->
    let
        hereAttempt =
          Proof.get idx proof
          |> Maybe.andThen (\subproof -> linearPred subproof idxs)
          |> Maybe.map (\pathTail -> idx :: pathTail)

        predAttempt () =
          Proof.get (idx - 1) proof
            |> Maybe.andThen (\subproof -> case subproof of
              ProofLine _ -> Just [idx - 1]
              ProofBlock _ _ -> pathToLastLine subproof |> Maybe.map (\pathTail -> (idx - 1) :: pathTail))

    in case hereAttempt of
      Just _ -> hereAttempt
      Nothing -> predAttempt ()

toId : Path -> String
toId path = "path_" ++ String.join "_" (List.map String.fromInt path)

targetsFirstAssumption : Proofy a -> Path -> Bool
targetsFirstAssumption proof path = case path of
  [] -> False
  [idx] -> case proof of
    ProofLine _ -> False
    ProofBlock head body -> idx == -(List.length head)
  idx::idxs ->
    Proof.get idx proof
    |> Maybe.map (\subproof -> targetsFirstAssumption subproof idxs)
    |> Maybe.withDefault False

targetsLastAssumption : Proofy a -> Path -> Bool
targetsLastAssumption proof path = case path of
  [] -> False
  [idx] -> idx == -1
  idx::idxs ->
    Proof.get idx proof
    |> Maybe.map (\subproof -> targetsLastAssumption subproof idxs)
    |> Maybe.withDefault False

into : Proofy a -> Path -> Maybe (Proofy a)
into proof path = case path of
  [] -> Just proof
  idx::idxs -> Proof.get idx proof |> Maybe.andThen (\subproof -> into subproof idxs)

targetsEmptyLine : Proofy String -> Path -> Bool
targetsEmptyLine proof path = case into proof path of
  Just (ProofLine "") -> True
  _ -> False

targetsBody : Path -> Bool
targetsBody path =
  ListUtil.last path
  |> Maybe.map (\lastIdx -> lastIdx >= 0)
  |> Maybe.withDefault False
