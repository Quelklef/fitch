module Decorate exposing (..)

import ListUtil

import Types exposing (Proofy(..), Path, Formula, Lineno, Knowledge, KnowledgeBox(..), DecoratedLine)
import Path
import Proof
import Formula
import Semantics

decorate : Proofy String -> Proofy DecoratedLine
decorate = decorate_ (1, [], []) >> Tuple.first

decorate_ : (Lineno, Path, Knowledge) -> Proofy String -> (Proofy DecoratedLine, Lineno)
decorate_ (lineno, path, knowledge) proof = case proof of
  ProofLine text ->
    let formula = Formula.parse text
        decorated =
          { text = text
          , formula = formula
          , path = path
          , lineno = lineno
          , knowledge = KnowledgeBox knowledge
          , justification =
              formula
              |> Result.fromMaybe "malformed"
              |> Result.andThen (\justFormula ->
              Semantics.verifySemantics knowledge justFormula
              |> Result.andThen (\() ->
              Semantics.justify knowledge justFormula
              |> Result.fromMaybe "invalid"))
          }
    in (ProofLine decorated, (lineno + 1))

  ProofBlock head body ->
    let (decoratedHead, lineno2, knowledge2) = decorateHead (0, lineno, (path, knowledge)) (List.reverse head)  -- << FIXME: reversed head
        (decoratedBody, lineno3) = decorateBody (0, lineno2, (path, knowledge2)) body
    in (ProofBlock (List.reverse decoratedHead) decoratedBody, lineno3)  -- << FIXME: reversed head

decorateHead : (Int, Lineno, (Path, Knowledge)) -> List String -> (List DecoratedLine, Lineno, Knowledge)
decorateHead (idx, lineno, (pathStub, knowledge)) lines = case lines of
  [] -> ([], lineno, knowledge)
  lineText::restLines ->
    let formula = Formula.parse lineText
        decoratedLine =
          { text = lineText
          , formula = formula
          , path = pathStub ++ [-idx-1]
          , lineno = lineno
          , knowledge = KnowledgeBox knowledge
          , justification =
              formula
              |> Result.fromMaybe "malformed"
              |> Result.andThen (Semantics.verifySemantics knowledge)
              |> Result.map (always "assumed")
          }
        (decoratedRest, lineno2, knowledge2) = restLines |> decorateHead (idx + 1, lineno + 1, (pathStub, knowledge ++ [ProofLine decoratedLine]))
    in (decoratedLine :: decoratedRest, lineno2, knowledge2)

decorateBody : (Int, Lineno, (Path, Knowledge)) -> List (Proofy String) -> (List (Proofy DecoratedLine), Lineno)
decorateBody (idx, lineno, (pathStub, knowledge)) proofs = case proofs of
  [] -> ([], lineno)
  first::rest ->
    let (decoratedFirst, lineno2) = decorate_ (lineno, pathStub ++ [idx], knowledge) first
        (decoratedRest, lineno3) = decorateBody (idx + 1, lineno2, (pathStub, knowledge ++ [decoratedFirst])) rest
    in (decoratedFirst :: decoratedRest, lineno3)
