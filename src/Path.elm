module Path exposing (..)

import List
import Maybe exposing (Maybe)

import ListUtil

import Types exposing (Proofy(..), Path)
import Proof

pretty : Path -> String
pretty = List.map String.fromInt >> String.join " → "

pathToLastLine : Proofy a -> Maybe Path
pathToLastLine proof = case proof of
  ProofLine _ -> Just []
  ProofBlock head body ->
    let maybeIdx =
          if List.length body > 0 then Just <| List.length body - 1
          else if List.length head > 0 then Just <| -(List.length head)
          else Nothing
    in maybeIdx
      |> Maybe.andThen (\idx -> Proof.get [idx] proof
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
      |> Maybe.andThen (\idx -> Proof.get [idx] proof
      |> Maybe.andThen (\subproof -> pathToFirstLine subproof)
      |> Maybe.map (\pathTail -> idx :: pathTail))

indexOfLastAssumption : Proofy a -> Maybe Int
indexOfLastAssumption proof = case proof of
  ProofLine _ -> Nothing
  ProofBlock head _ -> if List.length head == 0 then Nothing else Just -(List.length head)

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
          Proof.get [idx] proof
          |> Maybe.andThen (\subproof -> linearSucc subproof idxs)
          |> Maybe.map (\pathTail -> idx :: pathTail)

        -- vv If that fails, we'll look for the result in the succeeding index
        succAttempt () =
          let succIdx =
                if targetsLastAssumption proof [idx] then 0
                else if idx < 0 then idx - 1
                else idx + 1
          in Proof.get [succIdx] proof
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
          Proof.get [idx] proof
          |> Maybe.andThen (\subproof -> linearPred subproof idxs)
          |> Maybe.map (\pathTail -> idx :: pathTail)

        predAttempt () =
          let maybePredIdx =
                if idx == 0 then indexOfLastAssumption proof
                else if idx == -1 then Nothing
                else if idx < 0 then Just <| idx + 1
                else Just <| idx - 1
          in maybePredIdx
             |> Maybe.andThen (\predIdx -> Proof.get [predIdx] proof
             |> Maybe.andThen (\subproof -> case subproof of
               ProofLine _ -> Just [predIdx]
               ProofBlock _ _ -> pathToLastLine subproof |> Maybe.map (\pathTail -> predIdx :: pathTail)))

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
    Proof.get [idx] proof
    |> Maybe.map (\subproof -> targetsFirstAssumption subproof idxs)
    |> Maybe.withDefault False

targetsLastAssumption : Proofy a -> Path -> Bool
targetsLastAssumption proof path = case path of
  [] -> False
  [idx] -> case proof of
    ProofLine _ -> False
    ProofBlock head _ -> idx == -(List.length head)
  idx::idxs ->
    Proof.get [idx] proof
    |> Maybe.map (\subproof -> targetsLastAssumption subproof idxs)
    |> Maybe.withDefault False

targetsLastLine : Proofy a -> Path -> Bool
targetsLastLine proof path = case path of
  [] -> False
  [idx] -> case proof of
    ProofLine _ -> False
    ProofBlock head body ->
      List.length body == 0 && idx == -(List.length head)
      || idx == List.length body - 1
  idx::idxs ->
    Proof.get [idx] proof
    |> Maybe.map (\subproof -> targetsLastLine subproof idxs)
    |> Maybe.withDefault False

targetsEmptyLine : Proofy String -> Path -> Bool
targetsEmptyLine proof path = case Proof.get path proof of
  Just (ProofLine "") -> True
  _ -> False

targetsBody : Path -> Bool
targetsBody path =
  ListUtil.last path
  |> Maybe.map (\lastIdx -> lastIdx >= 0)
  |> Maybe.withDefault False

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
insertAfter : Bool -> Path -> a -> Proofy a -> Maybe (Proofy a)
insertAfter preferAssumption path line host =
  case path of
    [] -> Nothing

    [idx] ->
      case host of
        ProofLine _ -> Nothing
        ProofBlock head body ->
          -- vv Index targets last assumption
          if targetsLastAssumption host [idx] then
            if preferAssumption
            then Just <| ProofBlock (head ++ [line]) body
            else Just <| ProofBlock head (ProofLine line :: body)
          -- vv Index targets an assumption that is not the last one
          else if idx < 0 then
            ListUtil.insert (-idx-1 + 1) line head
            |> Maybe.map (\newHead -> ProofBlock newHead body)
          -- vv Index targets a body line
          else
            ListUtil.insert (idx + 1) (ProofLine line) body
            |> Maybe.map (\newBody -> ProofBlock head newBody)

    idx::idxs -> Proof.replaceM idx (insertAfter preferAssumption idxs line) host
