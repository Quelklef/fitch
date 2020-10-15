module Path exposing (..)

import Array
import Maybe exposing (Maybe)

import Proof exposing (Proofy(..))

-- vv Path to a formula in a proof, as a list of indicies
-- vv A negative index is an index into a block head; positive into the body
type alias Path = List Int

pathToLastLine : Proofy a -> Maybe Path
pathToLastLine proof = case proof of
  ProofLine _ -> Just []
  ProofBlock head body ->
    let maybeIdx =
          if Array.length body > 0 then Just <| Array.length body - 1
          else if Array.length head > 0 then Just <| -(Array.length head - 1)-1
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
          if Array.length head > 0 then Just -1
          else if Array.length body > 0 then Just 0
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
-- vv If the index is in bounds and is followed linearly by another
-- vv line, evaluates to the path of the following line
-- vv Otherwise, evaluates to an out-of-bounds index.
linearSuccIdxOrOob : Proofy a -> Int -> Int
linearSuccIdxOrOob proof idx =
  if indexTargetsLastAssumption proof idx then 0
  else if idx < 0 then idx - 1
  else idx + 1

-- vv Like linearSuccIdxOrOob, but in the opposite direction
linearPredIdxOrOob : Proofy a -> Int -> Int
linearPredIdxOrOob proof idx =
  let headLen = case proof of
        ProofLine _ -> 1
        ProofBlock head body -> Array.length head
      outOfBounds = case proof of
        ProofLine _ -> 1
        ProofBlock head body -> Array.length body
  in if idx == -1 then outOfBounds
  else if idx < 0 then idx + 1
  else if idx == 0 then -headLen
  else idx - 1

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
          let succIdx = linearSuccIdxOrOob proof idx
          in Proof.get succIdx proof
            |> Maybe.andThen (\subproof -> case subproof of
              ProofLine _ -> Just [succIdx]
              ProofBlock _ _ -> pathToFirstLine subproof |> Maybe.map (\pathTail -> succIdx :: pathTail))

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
          let predIdx = linearPredIdxOrOob proof idx
          in Proof.get predIdx proof
            |> Maybe.andThen (\subproof -> case subproof of
              ProofLine _ -> Just [predIdx]
              ProofBlock _ _ -> pathToLastLine subproof |> Maybe.map (\pathTail -> predIdx :: pathTail))

    in case hereAttempt of
      Just _ -> hereAttempt
      Nothing -> predAttempt ()

toId : Path -> String
toId path = "path_" ++ String.join "_" (List.map String.fromInt path)

-- vv Given an index and a proof, evaluates to whether or not
-- vv the subproof targeted by that index is, in particular, the
-- vv final assumption (ie the final line in the proof's head).
-- vv If the proof is a ProofLine instead of a ProofBlock,
-- vv evaluates to False.
indexTargetsLastAssumption : Proofy a -> Int -> Bool
indexTargetsLastAssumption proof idx = case proof of
  ProofLine line -> False
  ProofBlock head body -> idx == -(Array.length head)

-- vv Like indexTargetsLastAssumption, but for paths
targetsLastAssumption : Proofy a -> Path -> Bool
targetsLastAssumption proof path = case path of
  [] -> False
  [idx] -> indexTargetsLastAssumption proof idx
  idx::idxs ->
    Proof.get idx proof
    |> Maybe.map (\subproof -> targetsLastAssumption subproof idxs)
    |> Maybe.withDefault False
