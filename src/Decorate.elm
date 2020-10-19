module Decorate exposing (..)

import ListUtil

import Path exposing (Path)
import Proof exposing (Proofy(..))
import Formula exposing (Formula)
import Justify exposing (Lineno, Knowledge)

type alias DecoratedLine =
  { text : String
  , formula : Maybe Formula
  , path : Path
  , lineno : Lineno
  , justification : Maybe String
  }

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
          , justification = formula |> Maybe.andThen (Justify.justify knowledge)
          }
    in (ProofLine decorated, (lineno + 1))

  ProofBlock head body ->
    let decoratedHead = head |> List.indexedMap (\idx text ->
            { text = text
            , formula = Formula.parse text
            , path = path ++ [-idx-1]
            , lineno = lineno + (List.length head - idx - 1)
            , justification = Just "as"
            })

        headKnowledge = head
          |> List.indexedMap (\idx text -> (lineno + idx, text))
          |> List.filterMap (\(lineno_, text) -> Formula.parse text |> Maybe.map (\formula -> (lineno_, formula)))
          |> List.map ProofLine
        newKnowledge = knowledge ++ headKnowledge
        newLineno = lineno + List.length head
        (decoratedBody, lineno2) = decorateList_ (newLineno, path ++ [0], newKnowledge) body

    in (ProofBlock decoratedHead decoratedBody, lineno2)

decorateList_ : (Lineno, Path, Knowledge) -> List (Proofy String) -> (List (Proofy DecoratedLine), Lineno)
decorateList_ (lineno, path, knowledge) proofs = case proofs of
  [] -> ([], lineno)
  head::rest ->
    let (decoratedHead, lineno2) = decorate_ (lineno, path, knowledge) head
        (decoratedRest, lineno3) = decorateList_ (lineno2, naiveLinearSucc path, knowledge) rest
    in (decoratedHead :: decoratedRest, lineno3)

naiveLinearSucc : Path -> Path
naiveLinearSucc path = case path of
  [] -> []  -- GIGO, sorry
  [idx] -> [idx + 1]
  idx::idxs -> idx :: naiveLinearSucc idxs
