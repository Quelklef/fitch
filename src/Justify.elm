module Justify exposing (..)

import List
import Tuple

import Path exposing (Path)
import Proof exposing (Proofy(..))
import Formula exposing (Formula(..))

import ListUtil

type alias Lineno = Int

-- vv A 'context' or 'scope' of all the proofs available for a particular line to use in justification
type alias Knowledge = List (Proofy { lineno : Lineno, formula : Formula })

justify : Knowledge -> Formula -> Maybe String
justify knowledge goal =
  let strategies =
        [ justifyReiteration
        , justifyAndIntro
        ]
  in strategies |> ListUtil.findMapM (\strategy -> strategy knowledge goal)

type alias Strategy = Knowledge -> Formula -> Maybe String

justifyReiteration : Strategy
justifyReiteration knowledge goal =
  knowledge
  |> ListUtil.findMapM (\known -> case known of
    ProofBlock _ _ -> Nothing
    ProofLine { lineno, formula } ->
      if formula == goal
      then Just ("RI:" ++ String.fromInt lineno)
      else Nothing)

-- vv Give the line number of an already-known formula, if it's in the knowledge
linenoOf : Knowledge -> Formula -> Maybe Int
linenoOf knowledge target =
  knowledge
  |> ListUtil.findMapM (\proof ->
    case proof of
      ProofLine { lineno, formula } ->
        if formula == target
        then Just lineno
        else Nothing
      _ -> Nothing)

justifyAndIntro : Strategy
justifyAndIntro knowledge goal =
  case goal of
    And lhs rhs ->
      let lhsLineno = linenoOf knowledge lhs
          rhsLineno = linenoOf knowledge rhs
      in case (lhsLineno, rhsLineno) of
        (Just l, Just r) -> Just <| "&I:" ++ String.fromInt l ++ "," ++ String.fromInt r
        _ -> Nothing
    _ -> Nothing

