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
        , justifyConjunctionIntro
        , justifyConjunctionElim
        , justifyDisjunctionIntro
        , justifyDisjunctionElim
        , justifyImplicationIntro
        , justifyImplicationElim
        , justifyBiconditionalIntro
        , justifyBiconditionalElim
        , justifyBottomIntro
        , justifyNegationIntro
        , justifyNegationElim
        -- , justifyForallIntro
        -- , justifyForallElim
        -- , justifyExistsIntro
        -- , justifyExistsElim
        -- , justifyDomainNonEmpty
        -- , justifyEqualityIntro
        -- , justifyEqualityElim
        ]
  in strategies |> ListUtil.findMapM (\strategy -> strategy knowledge goal)

-- vv Gives a string representation of the range that a proof spans
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

-- vv Evaluates to a string representing the line number range
-- vv of given formula, if it is known
rangeOfKnownFormula : Formula -> Knowledge -> Maybe String
rangeOfKnownFormula target knowledge =
  knowledge
  |> List.filter (\proof ->
    case proof of
      ProofLine line -> line.formula == Just target
      _ -> False)
  |> List.head
  |> Maybe.map rangeOf

statements : Knowledge -> List DecoratedLine
statements = List.filterMap <| \known ->
  case known of
    ProofLine line -> Just line
    ProofBlock _ _ -> Nothing

blocks : Knowledge -> List (Proofy DecoratedLine)
blocks = List.filter (\known -> case known of
  ProofBlock _ _ -> True
  _ -> False)

justifyReiteration : Strategy
justifyReiteration knowledge goal =
  rangeOfKnownFormula goal knowledge
  |> Maybe.map (\range -> "RI:" ++ range)

justifyConjunctionIntro : Strategy
justifyConjunctionIntro knowledge goal =
  case goal of
    Conjunction lhs rhs ->
      let lhsRange = rangeOfKnownFormula lhs knowledge
          rhsRange = rangeOfKnownFormula rhs knowledge
      in case (lhsRange, rhsRange) of
        (Just l, Just r) -> Just <| "&I:" ++ l ++ "," ++ r
        _ -> Nothing
    _ -> Nothing

justifyConjunctionElim : Strategy
justifyConjunctionElim knowledge goal =
  knowledge
  |> List.filter (\known ->
    case known of
      ProofBlock _ _ -> False
      ProofLine line -> case line.formula of
        Just (Conjunction lhs rhs) -> lhs == goal || rhs == goal
        _ -> False)
  |> List.head
  |> Maybe.map (\conjunction -> "&E:" ++ rangeOf conjunction)

justifyDisjunctionIntro : Strategy
justifyDisjunctionIntro knowledge goal =
  case goal of
    Disjunction lhs rhs ->
      let lhsRange = rangeOfKnownFormula lhs knowledge
          rhsRange = rangeOfKnownFormula rhs knowledge
      in
        lhsRange |> MaybeUtil.orElse rhsRange
        |> Maybe.map (\range -> "|I:" ++ range)
    _ -> Nothing

justifyDisjunctionElim : Strategy
justifyDisjunctionElim knowledge goal =
  let disjunctions = knowledge |> List.filterMap (\known -> case known of
          ProofLine line -> case line.formula of
            Just (Disjunction lhs rhs) -> Just (known, lhs, rhs)
            _ -> Nothing
          _ -> Nothing)
  in Iter.product3 (Iter.fromList disjunctions) (Iter.fromList <| blocks knowledge) (Iter.fromList <| blocks knowledge)
     |> Iter.find (\((disjunction, lhs, rhs), blockA, blockB) ->
             (Proof.conclusion blockA |> Maybe.andThen .formula) == Just goal
             && (Proof.conclusion blockB |> Maybe.andThen .formula) == Just goal
             && (Proof.assumptions blockA |> List.map .formula) == [Just lhs]
             && (Proof.assumptions blockB |> List.map .formula) == [Just rhs])
     |> Maybe.map (\((disjunction, _, _), blockA, blockB) -> "|E:" ++ rangeOf disjunction ++ "," ++ rangeOf blockA ++ "," ++ rangeOf blockB)

justifyImplicationIntro : Strategy
justifyImplicationIntro knowledge goal =
  case goal of
    Implication lhs rhs ->
      knowledge
      |> List.filter (\known ->
        (Proof.assumptions known |> List.map .formula) == [Just lhs]
        && (Proof.conclusion known |> Maybe.andThen .formula) == Just rhs)
      |> List.head
      |> Maybe.map (\block -> "->I:" ++ rangeOf block)
    _ -> Nothing

justifyImplicationElim : Strategy
justifyImplicationElim knowledge goal =
  let implications = knowledge |> List.filterMap (\known -> case known of
        ProofLine line -> case line.formula of
          Just (Implication lhs rhs) -> Just (known, lhs, rhs)
          _ -> Nothing
        _ -> Nothing)
  in Iter.product (Iter.fromList implications) (Iter.fromList <| statements knowledge)
     |> Iter.find (\((implication, lhs, rhs), statement) -> statement.formula == Just lhs && goal == rhs)
     |> Maybe.map (\((implication, _, _), statement) -> "->E:" ++ rangeOf implication ++ "," ++ rangeOf (ProofLine statement))

justifyBiconditionalIntro : Strategy
justifyBiconditionalIntro knowledge goal =
  case goal of
    Biconditional lhs rhs ->
      Iter.product (Iter.fromList <| blocks knowledge ) (Iter.fromList <| blocks knowledge )
      |> Iter.find (\(blockA, blockB) ->
        (Proof.assumptions blockA |> List.map .formula) == [Just lhs]
        && (Proof.conclusion blockA |> Maybe.andThen .formula) == Just rhs
        && (Proof.assumptions blockB |> List.map .formula) == [Just rhs]
        && (Proof.conclusion blockB |> Maybe.andThen .formula) == Just lhs)
      |> Maybe.map (\(blockA, blockB) -> "<->I:" ++ rangeOf blockA ++ "," ++ rangeOf blockB)
    _ -> Nothing

justifyBiconditionalElim : Strategy
justifyBiconditionalElim knowledge goal =
  let biconditionals = knowledge |> List.filterMap (\known -> case known of
        ProofLine line -> case line.formula of
          Just (Biconditional lhs rhs) -> Just (known, lhs, rhs)
          _ -> Nothing
        _ -> Nothing)
  in Iter.product (Iter.fromList biconditionals) (Iter.fromList <| statements knowledge)
     |> Iter.find (\((biconditional, lhs, rhs), statement) -> statement.formula == Just lhs && goal == rhs
                                                           || statement.formula == Just rhs && goal == lhs)
     |> Maybe.map (\((biconditional, lhs, rhs), statement) -> "<->E:" ++ rangeOf biconditional ++ "," ++ rangeOf (ProofLine statement))

justifyBottomIntro : Strategy
justifyBottomIntro knowledge goal =
  knowledge
  |> ListUtil.find (\known ->
    case known of
      ProofLine line -> case line.formula of
        Just (Conjunction lhs rhs) -> lhs == Negation rhs || rhs == Negation lhs
        _ -> False
      _ -> False)
  |> Maybe.map (\line -> "#I:" ++ rangeOf line)

justifyNegationIntro : Strategy
justifyNegationIntro knowledge goal =
  case goal of
    Negation negated ->
      blocks knowledge
      |> List.filter (\block -> (Proof.assumptions block |> List.map .formula) == [Just negated]
                             && (Proof.conclusion block |> Maybe.andThen .formula) == Just Bottom)
      |> List.head
      |> Maybe.map (\block -> "~I:" ++ rangeOf block)
    _ -> Nothing

justifyNegationElim : Strategy
justifyNegationElim knowledge goal =
  knowledge
  |> ListUtil.find (\known ->
    case known of
      ProofLine line -> case line.formula of
        Just (Negation (Negation body)) -> body == goal
        _ -> False
      _ -> False)
  |> Maybe.map (\line -> "~E:" ++ rangeOf line)
