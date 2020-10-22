module Update exposing (..)

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
import Symbols

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
    SetProofTo newProof -> ({ model | proof = newProof }, Cmd.none)
    Noop -> (model, Cmd.none)
    SetFocusTo path -> (model, setFocusTo path)
    SetFormulaAt path newFormula       -> fromDo <| doSetFormulaAt path newFormula model.proof
    NewLineAfter path preferAssumption -> fromDo <| doNewLineAfter path preferAssumption model.proof
    IndentAt path                      -> fromDo <| doIndentAt path model.proof
    DedentAt path                      -> fromDo <| doDedentAt path model.proof
    BackspaceAt path                   -> fromDo <| doBackspaceAt path model.proof

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
doNewLineAfter : Path -> Bool -> Proofy String -> Maybe (Proofy String, Cmd Message)
doNewLineAfter path preferAssumption proof =
  doNewLineAfter_ path preferAssumption proof
  |> Maybe.andThen (\newProof ->
    path |> ListUtil.mapLast (\idx ->
        if Path.targetsLastAssumption newProof path then
          if preferAssumption then -1 else 0
        else if idx < 0 then idx
        else idx + 1)
    |> Maybe.map (\newPath -> (newProof, setFocusTo newPath)))

doNewLineAfter_ : Path -> Bool -> Proofy String -> Maybe (Proofy String)
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
  |> Maybe.map (\newProof -> (newProof, setFocusTo <| ListUtil.dropLast path))

doDedentAt_ : Path -> Proofy String -> Maybe (Proofy String)
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
    |> Maybe.andThen (\newProof -> case Proof.get idx newProof of
      Just (ProofBlock head body) ->
        if List.length head == 0 && List.length body == 0
        then Proof.remove idx newProof
        else Just newProof
      _ -> Just newProof)
