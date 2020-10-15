module Path exposing (..)

import Array
import Maybe exposing (Maybe)

import Proof exposing (Proofy(..))

-- vv Path to a formula in a proof, as a list of indicies
-- vv A negative index is an index into a block head; positive into the body
type alias Path = List Int

-- vv Give the linearly next path in a proof, ignoring nesting
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
-- vv (Currently naively assumes that the result will be in the same ProofBlock as the input)
linearSucc : Proofy a -> Path -> Maybe Path
linearSucc proof path = case path of
  [] -> Nothing
  [idx] ->
    let newIdx =
          if indexTargetsLastAssumption proof idx then 0
          else if idx < 0 then idx - 1
          else idx + 1
    in Just [newIdx]
  idx::idxs ->
    Proof.get idx proof
    |> Maybe.andThen (\subproof -> linearSucc subproof idxs)
    |> Maybe.map (\tail -> idx :: tail)

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
