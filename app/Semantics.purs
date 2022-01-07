module Fitch.Semantics where

import Prelude
import Data.Set as Set
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe (..))
import Data.String.CodePoints as String
import Data.String.CodePoints (CodePoint)
import Data.Either (Either (..))

import Fitch.Types (Proofy(..), Formula(..), Knowledge, DecoratedLine)
import Fitch.Proof as Proof
import Fitch.Formula as Formula
import Fitch.Util.Iter as Iter
import Fitch.Util.ArrayUtil as ArrayUtil
import Fitch.Util.MaybeUtil as MaybeUtil

verifySemantics :: Knowledge -> Formula -> Either String Unit
verifySemantics knowledge formula =
  let semanticChecks =
        [ checkNoUndeclaredFreeVars
        , checkQuantifiedNamesNotUsedAsPredicatesOrPropositions
        , checkNoShadowingInFormulas
        , checkNoShadowingInNestedBlocks
        ]
  in case ArrayUtil.findMapM (\check -> check knowledge formula) semanticChecks of
    Just err -> Left err
    Nothing -> Right unit

-- ↓ Checks that all free variabless are declared
checkNoUndeclaredFreeVars :: Knowledge -> Formula -> Maybe String
checkNoUndeclaredFreeVars knowledge formula =
  Formula.freeObjectVars formula # Set.toUnfoldable # ArrayUtil.findMapM (\varName ->
    let isDeclared = knowledge # Array.any (\known -> case known of
          ProofLine line -> case line.formula of
            Just (Declaration declaredName) -> declaredName == varName
            _ -> false
          _ -> false)
    in if isDeclared
       then Nothing
       else Just $ "'" <> String.singleton varName <> "' is free")

-- ↓ Gives an error on e.g. '∀PPa'
checkQuantifiedNamesNotUsedAsPredicatesOrPropositions :: Knowledge -> Formula -> Maybe String
checkQuantifiedNamesNotUsedAsPredicatesOrPropositions _knowledge formula =
  formula
  # scopedFirstJust (\scope subformula ->
    case subformula of
      Application name args ->
        let kind = if Array.length args == 0 then "proposition" else "predicate"
        in Set.member name scope
        # MaybeUtil.fromBool ("can't quantify over " <> kind <> " '" <> String.singleton name <> "'")
      _ -> Nothing)

-- ↓ Looks for variable shadowing within formulas
-- ↓ Such as in '∀x∃xPx'
checkNoShadowingInFormulas :: Knowledge -> Formula -> Maybe String
checkNoShadowingInFormulas _knowledge formula =
  formula
  # scopedFirstJust (\scope subformula ->
    Set.intersection scope (Formula.declaring subformula)
    # Set.toUnfoldable
    # Array.head
    # map (\name -> "'" <> String.singleton name <> "' is shadowed"))

-- ↓ Given a function and a formula, recursively call the function on all
-- ↓ subformulas with the variable scope and current formula;
-- ↓ Return the first Just returned.
scopedFirstJust :: forall a. (Set.Set CodePoint -> Formula -> Maybe a) -> Formula -> Maybe a
scopedFirstJust = scopedFirstJustAux Set.empty

scopedFirstJustAux :: forall a. Set.Set CodePoint -> (Set.Set CodePoint -> Formula -> Maybe a) -> Formula -> Maybe a
scopedFirstJustAux scope getMaybe formula =
  case getMaybe scope formula of
    Just x -> Just x
    Nothing -> Formula.children formula
               # map (scopedFirstJustAux (Set.union scope (Formula.declaring formula)) getMaybe)
               # ArrayUtil.firstJust
  
-- ↓ Looks for variable shadowing within nested blocks
-- ↓ Such as in:
-- ↓ │ 1. ∀x∀yPxy     assumed
-- ↓ ├──────────
-- ↓ ││ 2. [a]        assumed
-- ↓ │├──────────
-- ↓ ││ 3. ∀yPay      ∀∃:1[x→a]
-- ↓ │││ 4. [a]       assumed
-- ↓ ││├──────────
-- ↓ │││ 5. Paa       ∀∃:3[y→a]
checkNoShadowingInNestedBlocks :: Knowledge -> Formula -> Maybe String
checkNoShadowingInNestedBlocks knowledge formula =
  case formula of
    Declaration innerDeclName ->
      knowledge
      # ArrayUtil.findMapM (\proof -> case proof # Proof.map _.formula of
        ProofLine (Just (Declaration outerDeclName)) ->
            outerDeclName == innerDeclName
            # MaybeUtil.fromBool ("'" <> String.singleton outerDeclName <> "' is shadowed")
        _ -> Nothing)
    _ -> Nothing

-- --

justify :: Knowledge -> Formula -> Maybe String
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
  in justificationStrategies # ArrayUtil.findMapM (\strategy -> strategy knowledge goal)

-- ↓ Gives a string representation of the range that a proof spans
rangeOf :: Proofy DecoratedLine -> String
rangeOf proof = case proof of
  ProofLine { lineno } -> show lineno
  ProofBlock _ _ ->
    let firstLineNumber = Proof.firstLine proof <#> _.lineno
        lastLineNumber = Proof.lastLine proof <#> _.lineno
    in case firstLineNumber /\ lastLineNumber of
      Just from /\ Just to -> show from <> "-" <> show to
      _ -> "??"

type Strategy = Knowledge -> Formula -> Maybe String

-- ↓ Evaluates to a string representing the line number range
-- ↓ of given formula, if it is known
rangeOfKnownFormula :: Formula -> Knowledge -> Maybe String
rangeOfKnownFormula target knowledge =
  knowledge
  # Array.filter (\proof ->
    case proof of
      ProofLine line -> line.formula == Just target
      _ -> false)
  # Array.head
  # map rangeOf

statements :: Knowledge -> Array DecoratedLine
statements = Array.mapMaybe $ \known ->
  case known of
    ProofLine line -> Just line
    ProofBlock _ _ -> Nothing

blocks :: Knowledge -> Array (Proofy DecoratedLine)
blocks = Array.filter (\known -> case known of
  ProofBlock _ _ -> true
  _ -> false)

-- --

justifyEmpty :: Strategy
justifyEmpty _knowledge goal = case goal of
  Empty -> Just ""
  _ -> Nothing

justifyReiteration :: Strategy
justifyReiteration knowledge goal =
  rangeOfKnownFormula goal knowledge
  # map (\range -> "RI:" <> range)

justifyConjunctionIntro :: Strategy
justifyConjunctionIntro knowledge goal =
  case goal of
    Conjunction lhs rhs ->
      let lhsRange = rangeOfKnownFormula lhs knowledge
          rhsRange = rangeOfKnownFormula rhs knowledge
      in case lhsRange /\ rhsRange of
        Just l /\ Just r -> Just $ "∧I:" <> l <> "," <> r
        _ -> Nothing
    _ -> Nothing

justifyConjunctionElim :: Strategy
justifyConjunctionElim knowledge goal =
  knowledge
  # Array.filter (\known ->
    case known of
      ProofBlock _ _ -> false
      ProofLine line -> case line.formula of
        Just (Conjunction lhs rhs) -> lhs == goal || rhs == goal
        _ -> false)
  # Array.head
  # map (\conjunction -> "∧E:" <> rangeOf conjunction)

justifyDisjunctionIntro :: Strategy
justifyDisjunctionIntro knowledge goal =
  case goal of
    Disjunction lhs rhs ->
      let lhsRange = rangeOfKnownFormula lhs knowledge
          rhsRange = rangeOfKnownFormula rhs knowledge
      in
        lhsRange # MaybeUtil.orElse rhsRange
        # map (\range -> "∨I:" <> range)
    _ -> Nothing

justifyDisjunctionElim :: Strategy
justifyDisjunctionElim knowledge goal =
  let disjunctions = knowledge # Array.mapMaybe (\known -> case known of
          ProofLine line -> case line.formula of
            Just (Disjunction lhs rhs) -> Just $ known /\ lhs /\ rhs
            _ -> Nothing
          _ -> Nothing)
  in Iter.product3 (Iter.fromArray disjunctions) (Iter.fromArray $ blocks knowledge) (Iter.fromArray $ blocks knowledge)
     # Iter.find (\((_disjunction /\ lhs /\ rhs) /\ blockA /\ blockB) ->
             (Proof.conclusion blockA >>= _.formula) == Just goal
             && (Proof.conclusion blockB >>= _.formula) == Just goal
             && (Proof.assumptions blockA <#> _.formula) == [Just lhs]
             && (Proof.assumptions blockB <#> _.formula) == [Just rhs])
     # map (\((disjunction /\ _ /\ _) /\ blockA /\ blockB) -> "∨E:" <> rangeOf disjunction <> "," <> rangeOf blockA <> "," <> rangeOf blockB)

justifyImplicationIntro :: Strategy
justifyImplicationIntro knowledge goal =
  case goal of
    Implication lhs rhs ->
      knowledge
      # Array.filter (\known ->
        (Proof.assumptions known <#> _.formula) == [Just lhs]
        && (Proof.conclusion known >>= _.formula) == Just rhs)
      # Array.head
      # map (\block -> "→I:" <> rangeOf block)
    _ -> Nothing

justifyImplicationElim :: Strategy
justifyImplicationElim knowledge goal =
  let implications = knowledge # Array.mapMaybe (\known -> case known of
        ProofLine line -> case line.formula of
          Just (Implication lhs rhs) -> Just (known /\ lhs /\ rhs)
          _ -> Nothing
        _ -> Nothing)
  in Iter.product (Iter.fromArray implications) (Iter.fromArray $ statements knowledge)
     # Iter.find (\((_implication /\ lhs /\ rhs) /\ statement) -> statement.formula == Just lhs && goal == rhs)
     # map (\((implication /\ _ /\ _) /\ statement) -> "→E:" <> rangeOf implication <> "," <> rangeOf (ProofLine statement))

justifyBiconditionalIntro :: Strategy
justifyBiconditionalIntro knowledge goal =
  case goal of
    Biconditional lhs rhs ->
      Iter.product (Iter.fromArray $ blocks knowledge ) (Iter.fromArray $ blocks knowledge )
      # Iter.find (\(blockA /\ blockB) ->
        (Proof.assumptions blockA # map _.formula) == [Just lhs]
        && (Proof.conclusion blockA >>= _.formula) == Just rhs
        && (Proof.assumptions blockB # map _.formula) == [Just rhs]
        && (Proof.conclusion blockB >>= _.formula) == Just lhs)
      # map (\(blockA /\ blockB) -> "↔I:" <> rangeOf blockA <> "," <> rangeOf blockB)
    _ -> Nothing

justifyBiconditionalElim :: Strategy
justifyBiconditionalElim knowledge goal =
  let biconditionals = knowledge # Array.mapMaybe (\known -> case known of
        ProofLine line -> case line.formula of
          Just (Biconditional lhs rhs) -> Just (known /\ lhs /\ rhs)
          _ -> Nothing
        _ -> Nothing)
  in Iter.product (Iter.fromArray biconditionals) (Iter.fromArray $ statements knowledge)
     # Iter.find (\((_biconditional /\ lhs /\ rhs) /\ statement) ->
             statement.formula == Just lhs && goal == rhs
             || statement.formula == Just rhs && goal == lhs)
     # map (\((biconditional /\ _lhs /\ _rhs) /\ statement) ->
             "↔E:" <> rangeOf biconditional <> "," <> rangeOf (ProofLine statement))

justifyBottomIntro :: Strategy
justifyBottomIntro knowledge _goal =
  knowledge
  # ArrayUtil.find (\known ->
    case known of
      ProofLine line -> case line.formula of
        Just (Conjunction lhs rhs) -> lhs == Negation rhs || rhs == Negation lhs
        _ -> false
      _ -> false)
  # map (\line -> "⊥I:" <> rangeOf line)

justifyNegationIntro :: Strategy
justifyNegationIntro knowledge goal =
  case goal of
    Negation negated ->
      blocks knowledge
      # Array.filter (\block -> (Proof.assumptions block <#> _.formula) == [Just negated]
                             && (Proof.conclusion block >>= _.formula) == Just Bottom)
      # Array.head
      # map (\block -> "¬I:" <> rangeOf block)
    _ -> Nothing

justifyNegationElim :: Strategy
justifyNegationElim knowledge goal =
  knowledge
  # ArrayUtil.find (\known ->
    case known of
      ProofLine line -> case line.formula of
        Just (Negation (Negation body)) -> body == goal
        _ -> false
      _ -> false)
  # map (\line -> "¬E:" <> rangeOf line)

justifyForallIntro :: Strategy
justifyForallIntro knowledge goal =
  case goal of
    Forall forallName forallClaim ->
      blocks knowledge
      # ArrayUtil.findMapM (\block ->
        let assumptionMaybes = Proof.assumptions block # map _.formula
            maybeConclusion = Proof.conclusion block # map _.formula
        in case assumptionMaybes /\ maybeConclusion of
          ( [Just (Declaration blockDeclaringName)] /\ Just conclusion ) ->
            if (Formula.substitute blockDeclaringName forallName <$> conclusion) == Just forallClaim
            then Just ("∀I:" <> rangeOf block <> "[" <> String.singleton blockDeclaringName <> "→" <> String.singleton forallName <> "]")
            else Nothing
          _ -> Nothing)
    _ -> Nothing

justifyForallElim :: Strategy
justifyForallElim knowledge goal =
  knowledge
  # Iter.fromArray
  # Iter.filterMap (\known ->
    case known of
      ProofLine line -> case line.formula of
        Just (Forall forallName forallClaim) -> Just $ known /\ forallName /\ forallClaim
        _ -> Nothing
      _ -> Nothing)
  # (\x -> Iter.product x (Iter.fromSet $ Formula.freeObjectVars goal))
  # Iter.findMapM (\((forall' /\ forallName /\ forallClaim) /\ freeVar) ->
      goal == (forallClaim # Formula.substitute forallName freeVar)
      # MaybeUtil.fromBool ("∀E:" <> rangeOf forall' <> "[" <> String.singleton forallName <> "→" <> String.singleton freeVar <> "]"))

justifyExistsIntro :: Strategy
justifyExistsIntro knowledge goal =
  case goal of
    Exists existsName existsClaim ->
      statements knowledge
      # ArrayUtil.findMapM (\statement ->
         statement.formula
         >>= (\formula ->
         Formula.freeObjectVars formula
         # Iter.fromSet
         # Iter.findMapM (\freeVar ->
         (existsClaim # Formula.substitute existsName freeVar) == formula
         # MaybeUtil.fromBool ("∃I:" <> rangeOf (ProofLine statement) <> "[" <> String.singleton freeVar <> "→" <> String.singleton existsName <> "]" ))))
    _ -> Nothing

justifyExistsElim :: Strategy
justifyExistsElim knowledge goal =
  Iter.product (Iter.fromArray $ statements knowledge) (Iter.fromArray $ blocks knowledge)
  # Iter.findMapM (\(statement /\ block) ->
    case statement.formula of
      Just (Exists existsName existsClaim) ->
        blockDeclarationNameAndAssumption block >>= (\(blockDeclaringName /\ blockAssumption) ->
            (existsClaim # Formula.substitute existsName blockDeclaringName) == blockAssumption
              && (Proof.conclusion block >>= _.formula) == Just goal
            # MaybeUtil.fromBool ("∃E:" <> rangeOf (ProofLine statement) <> "," <> rangeOf block))
      _ -> Nothing)

-- ↓ Given a block proof where:
-- ↓ * There are exactly 1 or 2 assumptions, and
-- ↓ * The first assumption is a declaration,
-- ↓ evaluates to (the name declared, the second assumption or Empty if there is none)
blockDeclarationNameAndAssumption :: Proofy DecoratedLine -> Maybe (CodePoint /\ Formula)
blockDeclarationNameAndAssumption block =
  case Proof.assumptions block <#> _.formula of
    [Just (Declaration name), Just assumption] -> Just $ name /\ assumption
    [Just (Declaration name)] -> Just $ name /\ Empty
    _ -> Nothing

justifyDomainNonempty :: Strategy
justifyDomainNonempty _knowledge goal =
  case goal of
    Exists _ Empty -> Just "NE"
    _ -> Nothing

justifyEqualityIntro :: Strategy
justifyEqualityIntro _knowledge goal =
  case goal of
    Equality lhs rhs -> lhs == rhs # MaybeUtil.fromBool "=I"
    _ -> Nothing

justifyEqualityElim :: Strategy
justifyEqualityElim knowledge goal =
  knowledge
  # Iter.fromArray
  # Iter.filterMap (\known -> case known of
    ProofLine line -> case line.formula of
      Just (Equality lhs rhs) -> Just $ known /\ lhs /\ rhs
      _ -> Nothing
    _ -> Nothing)
  # (\x -> Iter.product x (Iter.fromArray $ statements knowledge))
  # Iter.findMapM (\((equality /\ lhs /\ rhs) /\ statement) ->
    let tryReplacement fromName toName =
          (statement.formula <#> (Formula.substitute fromName toName)) == Just goal
          # MaybeUtil.fromBool (
                "=E:" <> rangeOf equality <> "," <> rangeOf (ProofLine statement)
                <> "[" <> String.singleton fromName <> "→" <> String.singleton toName <> "]")
    in tryReplacement lhs rhs # MaybeUtil.orElseLazy (\_ -> tryReplacement rhs lhs))
