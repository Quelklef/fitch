module Semantics exposing (..)

import Set
import List
import Tuple

import Types exposing (Path, Proofy(..), Formula(..), Knowledge, DecoratedLine)
import Proof
import Formula
import Iter

import ListUtil
import MaybeUtil

verifySemantics : Knowledge -> Formula -> Result String ()
verifySemantics knowledge formula =
  let semanticChecks =
        [ checkNoUndeclaredFreeVars
        , checkNotQuantifyingOverPropositions
        ]
  in case ListUtil.findMapM (\check -> check knowledge formula) semanticChecks of
    Just err -> Err err
    Nothing -> Ok ()

-- vv Checks that all free variabless are declared
checkNoUndeclaredFreeVars : Knowledge -> Formula -> Maybe String
checkNoUndeclaredFreeVars knowledge formula =
  Formula.freeObjectVars formula |> Set.toList |> ListUtil.findMapM (\varName ->
    let isDeclared = knowledge |> List.any (\known -> case known of
          ProofLine line -> case line.formula of
            Just (Declaration declaredName) -> declaredName == varName
            _ -> False
          _ -> False)
    in if isDeclared
       then Nothing
       else Just <| "'" ++ String.fromChar varName ++ "' is free")

checkNotQuantifyingOverPropositions : Knowledge -> Formula -> Maybe String
checkNotQuantifyingOverPropositions knowledge formula =
  Nothing

-- --

justify : Knowledge -> Formula -> Maybe String
justify knowledge goal =
  let justificationStrategies =
        [ justifyEmpty
        , justifyReiteration
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
        , justifyForallIntro
        , justifyForallElim
        , justifyExistsIntro
        , justifyExistsElim
        , justifyDomainNonempty
        , justifyEqualityIntro
        , justifyEqualityElim
        ]
  in justificationStrategies |> ListUtil.findMapM (\strategy -> strategy knowledge goal)

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

-- --

justifyEmpty : Strategy
justifyEmpty knowledge goal = case goal of
  Empty -> Just ""
  _ -> Nothing

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
        (Just l, Just r) -> Just <| "∧I:" ++ l ++ "," ++ r
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
  |> Maybe.map (\conjunction -> "∧E:" ++ rangeOf conjunction)

justifyDisjunctionIntro : Strategy
justifyDisjunctionIntro knowledge goal =
  case goal of
    Disjunction lhs rhs ->
      let lhsRange = rangeOfKnownFormula lhs knowledge
          rhsRange = rangeOfKnownFormula rhs knowledge
      in
        lhsRange |> MaybeUtil.orElse rhsRange
        |> Maybe.map (\range -> "∨I:" ++ range)
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
     |> Maybe.map (\((disjunction, _, _), blockA, blockB) -> "∨E:" ++ rangeOf disjunction ++ "," ++ rangeOf blockA ++ "," ++ rangeOf blockB)

justifyImplicationIntro : Strategy
justifyImplicationIntro knowledge goal =
  case goal of
    Implication lhs rhs ->
      knowledge
      |> List.filter (\known ->
        (Proof.assumptions known |> List.map .formula) == [Just lhs]
        && (Proof.conclusion known |> Maybe.andThen .formula) == Just rhs)
      |> List.head
      |> Maybe.map (\block -> "→I:" ++ rangeOf block)
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
     |> Maybe.map (\((implication, _, _), statement) -> "→E:" ++ rangeOf implication ++ "," ++ rangeOf (ProofLine statement))

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
      |> Maybe.map (\(blockA, blockB) -> "↔I:" ++ rangeOf blockA ++ "," ++ rangeOf blockB)
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
     |> Maybe.map (\((biconditional, lhs, rhs), statement) -> "↔E:" ++ rangeOf biconditional ++ "," ++ rangeOf (ProofLine statement))

justifyBottomIntro : Strategy
justifyBottomIntro knowledge goal =
  knowledge
  |> ListUtil.find (\known ->
    case known of
      ProofLine line -> case line.formula of
        Just (Conjunction lhs rhs) -> lhs == Negation rhs || rhs == Negation lhs
        _ -> False
      _ -> False)
  |> Maybe.map (\line -> "⊥I:" ++ rangeOf line)

justifyNegationIntro : Strategy
justifyNegationIntro knowledge goal =
  case goal of
    Negation negated ->
      blocks knowledge
      |> List.filter (\block -> (Proof.assumptions block |> List.map .formula) == [Just negated]
                             && (Proof.conclusion block |> Maybe.andThen .formula) == Just Bottom)
      |> List.head
      |> Maybe.map (\block -> "¬I:" ++ rangeOf block)
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
  |> Maybe.map (\line -> "¬E:" ++ rangeOf line)

justifyForallIntro : Strategy
justifyForallIntro knowledge goal =
  case goal of
    Forall forallName forallClaim ->
      blocks knowledge
      |> ListUtil.findMapM (\block ->
        let assumptionMaybes = Proof.assumptions block |> List.map .formula
            maybeConclusion = Proof.conclusion block |> Maybe.map .formula
        in case (assumptionMaybes, maybeConclusion) of
          ( [Just (Declaration blockDeclaringName)], Just conclusion ) ->
            if Maybe.map (Formula.substitute blockDeclaringName forallName) conclusion == Just forallClaim
            then Just ("∀I:" ++ rangeOf block ++ "[" ++ String.fromChar blockDeclaringName ++ "→" ++ String.fromChar forallName ++ "]")
            else Nothing
          _ -> Nothing)
    _ -> Nothing

justifyForallElim : Strategy
justifyForallElim knowledge goal =
  knowledge
  |> Iter.fromList
  |> Iter.filterMap (\known ->
    case known of
      ProofLine line -> case line.formula of
        Just (Forall forallName forallClaim) -> Just (known, forallName, forallClaim)
        _ -> Nothing
      _ -> Nothing)
  |> (\x -> Iter.product x (Iter.fromSet <| Formula.freeObjectVars goal))
  |> Iter.findMapM (\((forall, forallName, forallClaim), freeVar) ->
      goal == (forallClaim |> Formula.substitute forallName freeVar)
      |> MaybeUtil.fromBool ("∀E:" ++ rangeOf forall ++ "[" ++ String.fromChar forallName ++ "→" ++ String.fromChar freeVar ++ "]"))

justifyExistsIntro : Strategy
justifyExistsIntro knowledge goal =
  case goal of
    Exists existsName existsClaim ->
      statements knowledge
      |> ListUtil.findMapM (\statement ->
         statement.formula
         |> Maybe.andThen (\formula ->
         Formula.freeObjectVars formula
         |> Iter.fromSet
         |> Iter.findMapM (\freeVar ->
         (existsClaim |> Formula.substitute existsName freeVar) == formula
         |> MaybeUtil.fromBool ("∃I:" ++ rangeOf (ProofLine statement) ++ "[" ++ String.fromChar freeVar ++ "→" ++ String.fromChar existsName ++ "]" ))))
    _ -> Nothing

justifyExistsElim : Strategy
justifyExistsElim knowledge goal =
  Iter.product (Iter.fromList <| statements knowledge) (Iter.fromList <| blocks knowledge)
  |> Iter.findMapM (\(statement, block) ->
    case statement.formula of
      Just (Exists existsName existsClaim) ->
        case Proof.assumptions block |> List.map .formula of
          [Just (Declaration blockDeclaringName), Just blockAssumption] ->
            (existsClaim |> Formula.substitute existsName blockDeclaringName) == blockAssumption
              && (Proof.conclusion block |> Maybe.andThen .formula) == Just goal
            |> MaybeUtil.fromBool ("∃E:" ++ rangeOf (ProofLine statement) ++ "," ++ rangeOf block)
          _ -> Nothing
      _ -> Nothing)

justifyDomainNonempty : Strategy
justifyDomainNonempty knowledge goal =
  case goal of
    Exists _ Empty -> Just "NE"
    _ -> Nothing

justifyEqualityIntro : Strategy
justifyEqualityIntro knowledge goal =
  case goal of
    Equality lhs rhs -> lhs == rhs |> MaybeUtil.fromBool "=I"
    _ -> Nothing

justifyEqualityElim : Strategy
justifyEqualityElim knowledge goal =
  knowledge
  |> Iter.fromList
  |> Iter.filterMap (\known -> case known of
    ProofLine line -> case line.formula of
      Just (Equality lhs rhs) -> Just (known, lhs, rhs)
      _ -> Nothing
    _ -> Nothing)
  |> (\x -> Iter.product x (Iter.fromList <| statements knowledge))
  |> Iter.findMapM (\((equality, lhs, rhs), statement) ->
    let tryReplacement fromName toName =
          (statement.formula |> Maybe.map (Formula.substitute fromName toName)) == Just goal
          |> MaybeUtil.fromBool (
                "=E:" ++ rangeOf equality ++ "," ++ rangeOf (ProofLine statement)
                ++ "[" ++ String.fromChar fromName ++ "→" ++ String.fromChar toName ++ "]")
    in tryReplacement lhs rhs |> MaybeUtil.orElseLazy (\() -> tryReplacement rhs lhs))
