port module Update exposing (..)

import Browser.Dom as Dom
import Task
import Maybe exposing (Maybe)

import ListUtil
import StringUtil

import Types exposing (Model, Message(..), Path, Proofy(..), Formula)
import Path
import Proof
import Formula
import Decorate
import Semantics
import TextStyle
import Serialize

-- vv Replaces the url of the page without reloading
port setUrlArg : String -> Cmd msg

setFocusTo : Path -> Cmd Message
setFocusTo path = Task.attempt (always Noop) (Dom.focus <| Path.toId path)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
  let fromNewProofAndCommand maybeProofAndCommand = case maybeProofAndCommand of
        Just (newProof, cmd) -> ({ model | proof = newProof }, Cmd.batch [cmd, setUrlArg <| Serialize.serialize newProof])
        Nothing -> (model, Cmd.none)
  in case msg of
    ToggleDebugMode  -> ({ model | showDebugInfo = not model.showDebugInfo }, Cmd.none)
    ToggleUseUnicode -> ({ model | useUnicode = not model.useUnicode }, Cmd.none)
    Noop             -> (model, Cmd.none)
    SetFocusTo path  -> (model, setFocusTo path)
    SetProofTo newProof                -> fromNewProofAndCommand <| Just (newProof, Cmd.none)
    SetFormulaAt path newFormula       -> fromNewProofAndCommand <| doSetFormulaAt path newFormula model.proof
    NewLineAfter path preferAssumption -> fromNewProofAndCommand <| doNewLineAfter path preferAssumption model.proof
    IndentAt path                      -> fromNewProofAndCommand <| doIndentAt path model.proof
    DedentAt path                      -> fromNewProofAndCommand <| doDedentAt path model.proof
    BackspaceAt path                   -> fromNewProofAndCommand <| doBackspaceAt path model.proof

doSetFormulaAt : Path -> String -> Proofy String -> Maybe (Proofy String, Cmd Message)
doSetFormulaAt path newFormula proof =
  doSetFormulaAt_ path newFormula proof
  |> Maybe.map (\newProof -> (newProof, Cmd.none))

doSetFormulaAt_ : Path -> String -> Proofy String -> Maybe (Proofy String)
doSetFormulaAt_ path newFormula proof =
  case path of
    [] -> case proof of
      ProofBlock _ _ -> Nothing
      ProofLine  oldFormula -> Just (ProofLine newFormula)
    idx::idxs -> Proof.replaceM idx (\subproof -> doSetFormulaAt_ idxs newFormula subproof) proof

doNewLineAfter : Path -> Bool -> Proofy String -> Maybe (Proofy String, Cmd Message)
doNewLineAfter path preferAssumption proof =
  doNewLineAfter_ path preferAssumption proof
  |> Maybe.andThen (\newProof ->
    Path.linearSucc newProof path
    |> Maybe.map (\newPath -> (newProof, setFocusTo newPath)))

doNewLineAfter_ : Path -> Bool -> Proofy String -> Maybe (Proofy String)
doNewLineAfter_ path preferAssumption proof = Path.insertAfter preferAssumption path "" proof

doIndentAt : Path -> Proofy String -> Maybe (Proofy String, Cmd Message)
doIndentAt path proof =
  doIndentAt_ path proof
  |> Maybe.map (\newProof -> (newProof, setFocusTo <| path ++ [-0-1]))

doIndentAt_ : Path -> Proofy String -> Maybe (Proofy String)
doIndentAt_ path proof =
  case path of
    [] -> case proof of
      ProofLine line -> Just <| ProofBlock [line] []
      ProofBlock _ _ -> Nothing
    idx::idxs -> Proof.replaceM idx (\subproof -> doIndentAt_ idxs subproof) proof

doDedentAt : Path -> Proofy String -> Maybe (Proofy String, Cmd Message)
doDedentAt path proof =
  doDedentAt_ path proof
  |> Maybe.andThen (\newProof ->
    let targetHasOnlyOneLine =
          ListUtil.init path
          |> (\thePath -> Proof.get thePath proof)
          |> Maybe.map (\block -> Proof.length block == 1)
          |> Maybe.withDefault False
        maybeNewPath =
          if targetHasOnlyOneLine
          then ListUtil.dropLast path |> Just
          else path |> Path.linearPred proof |> Maybe.andThen (Path.linearSucc newProof)
    in maybeNewPath
       |> Maybe.map (\newPath -> (newProof, setFocusTo <| newPath)))

doDedentAt_ : Path -> Proofy String -> Maybe (Proofy String)
doDedentAt_ path proof =
  case path of
    [] -> Nothing
    [idx] -> Nothing

    [parentIdx, childIdx] -> case proof of
      ProofLine _ -> Nothing
      ProofBlock parentHead parentBody ->
        let parent = proof
            maybeChild = Proof.get [parentIdx] parent
            maybeChildLastLine = maybeChild |> Maybe.andThen Proof.lastLine
        in case (maybeChild, maybeChildLastLine) of
          (Just (ProofBlock childHead childBody), Just childLastLine) ->
             let child = ProofBlock childHead childBody
                 childHasOnlyOneLine = List.length childHead + List.length childBody == 1
                 targetsLastLine = Path.targetsLastLine child [childIdx]
             -- vv Allow dedenting a line only if it targets the last line of the proof
             in if not targetsLastLine then Nothing
                -- vv If only one line, collapse it down into parent proof
                else if childHasOnlyOneLine then Proof.replace parentIdx (always (ProofLine childLastLine)) parent
                -- vv If more than one line, need to collapse the last line
                -- vv down but leave the rest of the block intact
                else let newChild = Proof.remove childIdx child
                     in parent
                        |> Proof.replaceM parentIdx (always newChild)
                        |> Maybe.andThen (Path.insertAfter False [parentIdx] childLastLine)
          _ -> Nothing

    idx::idxs -> Proof.replaceM idx (\subproof -> doDedentAt_ idxs subproof) proof

doBackspaceAt : Path -> Proofy String -> Maybe (Proofy String, Cmd Message)
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

doBackspaceAt_ : Path -> Proofy String -> Maybe (Proofy String)
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
    |> Maybe.andThen (\newProof -> case Proof.get [idx] newProof of
      Just (ProofBlock head body) ->
        if List.length head == 0 && List.length body == 0
        then Proof.remove idx newProof
        else Just newProof
      _ -> Just newProof)
