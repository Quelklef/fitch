module Justify exposing (..)

import List
import Tuple

import Path exposing (Path)
import Proof exposing (Proofy(..))
import Formula exposing (Formula(..))
import Iter

import ListUtil
import MaybeUtil

type alias Lineno = Int

-- vv A 'context' or 'scope' of all the proofs available for a particular line to use in justification
type alias Knowledge = List (Proofy DecoratedLine)

type alias DecoratedLine =
  { text : String
  , formula : Maybe Formula
  , path : Path
  , lineno : Lineno
  , justification : Maybe String
  }

justify : Knowledge -> Formula -> Maybe String
justify knowledge goal =
  let strategies =
        [ justifyReiteration
        , justifyAndIntro
        , justifyAndElim
        , justifyOrIntro
        , justifyOrElim
        , justifyIfIntro
        , justifyIfElim
        ]
  in strategies |> ListUtil.findMapM (\strategy -> strategy knowledge goal)

rangeOf : Proofy DecoratedLine -> String
rangeOf proof = case proof of
  ProofLine { lineno } -> String.fromInt lineno
  ProofBlock _ _ ->
    let firstLineNumber = Proof.firstLine proof |> Maybe.map .lineno
        lastLineNumber = Proof.lastLine proof |> Maybe.map .lineno
    in case (firstLineNumber, lastLineNumber) of
      (Just from, Just to) -> String.fromInt from ++ "-" ++ String.fromInt to
      _ -> "??"

type alias Strategy = Knowledge -> Formula -> Maybe String

justifyReiteration : Strategy
justifyReiteration knowledge goal =
  knowledge
  |> ListUtil.findMapM (\known -> case known of
    ProofBlock _ _ -> Nothing
    ProofLine line ->
      if line.formula == Just goal
      then Just ("RI:" ++ rangeOf known)
      else Nothing)

findMapKnown : (Proofy DecoratedLine -> Maybe r) -> Knowledge -> List r
findMapKnown mapper knowledge = knowledge |> List.filterMap mapper

findKnown : (Proofy DecoratedLine -> Bool) -> Knowledge -> List (Proofy DecoratedLine)
findKnown predicate knowledge =
  knowledge |> List.filterMap (\proof -> predicate proof |> MaybeUtil.fromBool proof)

-- vv Evaluates to a string representing the line number range
-- vv of given formula, if it is known
rangeOfKnownFormula : Formula -> Knowledge -> Maybe String
rangeOfKnownFormula target knowledge =
  knowledge
  |> findKnown (\proof ->
    case proof of
      ProofLine line -> line.formula == Just target
      _ -> False)
  |> List.head
  |> Maybe.map rangeOf

justifyAndIntro : Strategy
justifyAndIntro knowledge goal =
  case goal of
    And lhs rhs ->
      let lhsRange = rangeOfKnownFormula lhs knowledge
          rhsRange = rangeOfKnownFormula rhs knowledge
      in case (lhsRange, rhsRange) of
        (Just l, Just r) -> Just <| "&I:" ++ l ++ "," ++ r
        _ -> Nothing
    _ -> Nothing

justifyAndElim : Strategy
justifyAndElim knowledge goal =
  let conjunction =
        knowledge
        |> findKnown (\known ->
          case known of
            ProofBlock _ _ -> False
            ProofLine line -> case line.formula of
              Just (And lhs rhs) -> lhs == goal || rhs == goal
              _ -> False)
        |> List.head

  in conjunction |> Maybe.map (\conj -> "&E:" ++ rangeOf conj)

justifyOrIntro : Strategy
justifyOrIntro knowledge goal =
  case goal of
    Or lhs rhs ->
      let lhsRange () = rangeOfKnownFormula lhs knowledge
          rhsRange () = rangeOfKnownFormula rhs knowledge
      in
        lhsRange () |> MaybeUtil.orLazy rhsRange
        |> Maybe.map (\lineno -> "|I:" ++ lineno)
    _ -> Nothing

justifyOrElim : Strategy
justifyOrElim knowledge goal =

  let blocks = knowledge |> findKnown (\known -> case known of
          ProofBlock _ _ -> True
          _ -> False)

      disjunctions = knowledge |> findMapKnown (\known -> case known of
          ProofLine line -> case line.formula of
            Just (Or lhs rhs) -> Just (known, lhs, rhs)
            _ -> Nothing
          _ -> Nothing)

  in Iter.product3 (Iter.fromList disjunctions) (Iter.fromList blocks) (Iter.fromList blocks)
     |> Iter.findMapM (\((disjunction, lhs, rhs), blockA, blockB) ->
         let isSufficient =
               (Proof.conclusion blockA |> Maybe.andThen .formula) == Just goal
               && (Proof.conclusion blockB |> Maybe.andThen .formula) == Just goal
               && (Proof.assumptions blockA |> List.map .formula) == [Just lhs]
               && (Proof.assumptions blockB |> List.map .formula) == [Just rhs]
         in isSufficient |> MaybeUtil.fromBool ("|E:" ++ rangeOf disjunction ++ "," ++ rangeOf blockA ++ "," ++ rangeOf blockB))

justifyIfIntro : Strategy
justifyIfIntro knowledge goal =
  case goal of
    If lhs rhs ->
      knowledge
      |> findKnown (\known ->
        (Proof.assumptions known |> List.map .formula) == [Just lhs]
        && (Proof.conclusion known |> Maybe.andThen .formula) == Just rhs)
      |> List.head
      |> Maybe.map rangeOf
      |> Maybe.map (\range -> "->I:" ++ range)
    _ -> Nothing

justifyIfElim : Strategy
justifyIfElim knowledge goal =
  let ifs = knowledge |> findMapKnown (\known -> case known of
        ProofLine line -> case line.formula of
          Just (If lhs rhs) -> Just (known, lhs, rhs)
          _ -> Nothing
        _ -> Nothing)
      statements = knowledge |> findMapKnown (\known -> case known of
        ProofLine line -> Just line
        _ -> Nothing)
  in Iter.product (Iter.fromList ifs) (Iter.fromList statements)
     |> Iter.find (\((implication, lhs, rhs), statement) -> statement.formula == Just lhs && goal == rhs)
     |> Maybe.map (\((implication, _, _), statement) -> "->E:" ++ rangeOf implication ++ "," ++ rangeOf (ProofLine statement))
