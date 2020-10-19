module Justify exposing (..)

import List
import Tuple

import Path exposing (Path)
import Proof exposing (Proofy(..))
import Formula exposing (Formula)

import ListUtil

type alias Lineno = Int

-- vv A 'context' or 'scope' of all the proofs available for a particular line to use in justification
type alias Knowledge = List (Proofy (Lineno, Formula))

justify : Knowledge -> Formula -> Maybe String
justify knowledge goal =
  let strategies =
        [ justifyReiteration
        ]
  in strategies |> ListUtil.findMapM (\strategy -> strategy knowledge goal)

type alias Strategy = Knowledge -> Formula -> Maybe String

justifyReiteration : Strategy
justifyReiteration knowledge goal =
  knowledge
  |> ListUtil.findMapM (\known -> case known of
    ProofBlock _ _ -> Nothing
    ProofLine (lineno, line) ->
      if line == goal
      then Just ("RI:" ++ String.fromInt lineno)
      else Nothing)

