module Decorate exposing (..)

import Array exposing (Array)

import ArrayUtil

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
decorate = decorate_ (0, [], []) >> Tuple.first

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
    let decoratedHead = head |> Array.indexedMap (\idx text ->
            { text = text
            , formula = Formula.parse text
            , path = path ++ [-idx-1]
            , lineno = lineno + idx
            , justification = Just "as"
            })

        headKnowledge = head
          |> Array.indexedMap (\idx text -> (lineno + idx, text))
          |> ArrayUtil.filterMap (\(lineno_, text) -> Formula.parse text |> Maybe.map (\formula -> (lineno_, formula)))
          |> Array.map ProofLine
          |> Array.toList
        newKnowledge = knowledge ++ headKnowledge
        newLineno = lineno + Array.length head
        (decoratedBody, lineno2) = decorateArray_ (newLineno, path ++ [0], newKnowledge) body

    in (ProofBlock decoratedHead decoratedBody, lineno2)

decorateArray_ : (Lineno, Path, Knowledge) -> Array (Proofy String) -> (Array (Proofy DecoratedLine), Lineno)
decorateArray_ (lineno, path, knowledge) proofs = case ArrayUtil.uncons proofs of
  Nothing -> (Array.empty, lineno)
  Just (head, rest) ->
    let (decoratedHead, lineno2) = decorate_ (lineno, path, knowledge) head
        (decoratedRest, lineno3) = decorateArray_ (lineno2, naiveLinearSucc path, knowledge) rest
    in (ArrayUtil.cons decoratedHead decoratedRest, lineno3)

naiveLinearSucc : Path -> Path
naiveLinearSucc path = case path of
  [] -> []  -- GIGO, sorry
  [idx] -> [idx + 1]
  idx::idxs -> idx :: naiveLinearSucc idxs
