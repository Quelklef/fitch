module Fitch.Semantics where

import Prelude
import Data.Set as Set
import Data.Set (Set)
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Either (Either (..))
import Data.Foldable (findMap, find, and, fold)
import Data.List.Lazy as LList
import Data.Monoid (guard)
import Control.Alt ((<|>))

import Fitch.Types (Proofy (..), Formula (..), Knowledge, DecoratedLine, Name_FOL, Name_Fitch, Name_Obj (..), getName)
import Fitch.Proof as Proof
import Fitch.Formula as Formula
import Fitch.Util.ArrayUtil as ArrayUtil
import Fitch.Util.MaybeUtil as MaybeUtil

verifySemantics :: Knowledge -> Formula -> Either String Unit
verifySemantics knowledge formula =
  let semanticChecks =
        [ checkNoUndeclaredFreeVars
        , checkNoShadowingInFormulas
        , checkNoShadowingInNestedBlocks
        ]
  in case semanticChecks # findMap (\check -> check knowledge formula) of
    Just err -> Left err
    Nothing -> Right unit

-- ↓ Checks that all free variables are declared
checkNoUndeclaredFreeVars :: Knowledge -> Formula -> Maybe String
checkNoUndeclaredFreeVars knowledge formula =
  Formula.freeVars formula # findMap (\varName ->
    guard (not $ varName `Set.member` declaredVars)
          (Just $ "'" <> getName varName <> "' is free"))

  where
  declaredVars = Set.fromFoldable $ do
    known <- knowledge
    line <- case known of
      ProofLine line -> [line]
      _ -> []
    var <- case line.formula of
      Declaration var -> [var]
      _ -> []
    pure $ var

scopedFirstJustGeneric :: forall m a. Monoid m => (Formula -> m) -> (m -> Formula -> Maybe a) -> Formula -> Maybe a
scopedFirstJustGeneric declaring = impl mempty
  where

  impl :: m -> (m -> Formula -> Maybe a) -> Formula -> Maybe a
  impl scope getMaybe formula =
    case getMaybe scope formula of
      Just x -> Just x
      Nothing -> Formula.children formula
                 # map (impl (scope <> declaring formula) getMaybe)
                 # ArrayUtil.firstJust

-- ↓ Looks for variable shadowing within formulas
-- ↓ Such as in '∀x∃xPx'
checkNoShadowingInFormulas :: Knowledge -> Formula -> Maybe String
checkNoShadowingInFormulas _knowledge formula =
  formula
  # scopedFirstJustGeneric declaring (\scope subformula ->
    Set.intersection scope (declaring subformula)
    # (Array.head <<< Set.toUnfoldable)
    # map (\name -> "'" <> getName name <> "' is shadowed"))

  where

  declaring :: Formula -> Set Name_FOL
  declaring = case _ of
    Forall name _ -> Set.singleton name
    Exists name _ -> Set.singleton name
    _ -> Set.empty

-- ↓ Given a function and a formula, recursively call the function on all
-- ↓ subformulas with the variable scope and current formula;
-- ↓ Return the first Just returned.
scopedFirstJust :: forall a. (Set Name_Fitch -> Formula -> Maybe a) -> Formula -> Maybe a
scopedFirstJust = scopedFirstJustGeneric declaring
  where

  declaring :: Formula -> Set Name_Fitch
  declaring = case _ of
    Declaration name -> Set.singleton name
    _ -> Set.empty
  
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
      # findMap (\proof -> case proof <#> _.formula of
        ProofLine (Declaration outerDeclName) ->
            guard (outerDeclName == innerDeclName)
                  (Just $ "'" <> getName outerDeclName <> "' is shadowed")
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
        , justifyEqualityIntro
        , justifyEqualityElim
        ]
  in justificationStrategies # findMap (\strategy -> strategy knowledge goal)

-- ↓ Gives a string representation of the range that a proof spans
rangeOf :: Proofy DecoratedLine -> String
rangeOf proof = case proof of
  ProofLine { lineno } -> show lineno
  ProofBlock _ _ -> fromMaybe "??" do
    from <- _.lineno <$> Proof.firstLine proof
    to <- _.lineno <$> Proof.lastLine proof
    pure $ show from <> "-" <> show to

type Strategy = Knowledge -> Formula -> Maybe String

-- ↓ Evaluates to a string representing the line number range
-- ↓ of given formula, if it is known
rangeOfKnownFormula :: Formula -> Knowledge -> Maybe String
rangeOfKnownFormula target knowledge =
  knowledge
  # Array.filter (\proof ->
    case proof of
      ProofLine line -> line.formula == target
      _ -> false)
  # Array.head
  # map rangeOf

statements :: Knowledge -> Array DecoratedLine
statements = Array.mapMaybe case _ of
  ProofLine line -> Just line
  ProofBlock _ _ -> Nothing

blocks :: Knowledge -> Array (Proofy DecoratedLine)
blocks = Array.filter case _ of
  ProofBlock _ _ -> true
  _ -> false

-- --

justifyEmpty :: Strategy
justifyEmpty _knowledge = case _ of
  Empty -> Just ""
  _ -> Nothing

justifyReiteration :: Strategy
justifyReiteration knowledge goal =
  ("RI:" <> _) <$> rangeOfKnownFormula goal knowledge

justifyConjunctionIntro :: Strategy
justifyConjunctionIntro knowledge goal =
  case goal of
    Conjunction lhs rhs -> do
      lhsRange <- rangeOfKnownFormula lhs knowledge
      rhsRange <- rangeOfKnownFormula rhs knowledge
      pure $ "∧I:" <> lhsRange <> "," <> rhsRange
    _ -> Nothing

justifyConjunctionElim :: Strategy
justifyConjunctionElim knowledge goal =
  knowledge # Array.findMap (case _ of
      ProofBlock _ _ -> Nothing
      pf@(ProofLine line) -> case line.formula of
        Conjunction lhs rhs ->
          if lhs == goal || rhs == goal
          then Just $ "∧E:" <> rangeOf pf
          else Nothing
        _ -> Nothing)

justifyDisjunctionIntro :: Strategy
justifyDisjunctionIntro knowledge goal =
  case goal of
    Disjunction lhs rhs -> let
      lhsRange = rangeOfKnownFormula lhs knowledge
      rhsRange = rangeOfKnownFormula rhs knowledge
      in ("∨I:" <> _) <$> (lhsRange <|> rhsRange)
    _ -> Nothing

justifyDisjunctionElim :: Strategy
justifyDisjunctionElim knowledge goal =

      pretty <$> find valid candidates

  where

    disjunctions = knowledge # Array.mapMaybe \known -> case known of
      ProofLine line -> case line.formula of
        Disjunction lhs rhs -> Just $ known /\ lhs /\ rhs
        _ -> Nothing
      _ -> Nothing

    candidates = do
      disj /\ lhs /\ rhs <- LList.fromFoldable disjunctions
      block1 <- LList.fromFoldable $ blocks knowledge
      block2 <- LList.fromFoldable $ blocks knowledge
      pure $ disj /\ lhs /\ rhs /\ block1 /\ block2

    valid (_ /\ lhs /\ rhs /\ blockA /\ blockB) =
      and [ (Proof.conclusion blockA <#> _.formula) == Just goal
          , (Proof.conclusion blockB <#> _.formula) == Just goal
          , (Proof.assumptions blockA <#> _.formula) == [lhs]
          , (Proof.assumptions blockB <#> _.formula) == [rhs]
          ]

    pretty (disj /\ _ /\ _ /\ blockA /\ blockB) =
      "∨E:" <> rangeOf disj <> "," <> rangeOf blockA <> "," <> rangeOf blockB

justifyImplicationIntro :: Strategy
justifyImplicationIntro knowledge goal = case goal of
  Implication lhs rhs ->
    knowledge # Array.findMap \known ->
      let valid = and
            [ (Proof.assumptions known <#> _.formula) == [lhs]
            , (Proof.conclusion known <#> _.formula) == Just rhs
            ]
      in guard valid (Just $ "→I:" <> rangeOf known)
  _ -> Nothing

justifyImplicationElim :: Strategy
justifyImplicationElim knowledge goal =

    pretty <$> find valid candidates

  where

    implications = knowledge # Array.mapMaybe (\known -> case known of
          ProofLine line -> case line.formula of
            Implication lhs rhs -> Just $ known /\ lhs /\ rhs
            _ -> Nothing
          _ -> Nothing)

    candidates = do
       im /\ lhs /\ rhs <- LList.fromFoldable implications
       st <- LList.fromFoldable (statements knowledge)
       pure $ im /\ lhs /\ rhs /\ st

    valid (_ /\ lhs /\ rhs /\ statement) =
      statement.formula == lhs && goal == rhs

    pretty (implication /\ _ /\ _ /\ statement) =
      "→E:" <> rangeOf implication <> "," <> rangeOf (ProofLine statement)

justifyBiconditionalIntro :: Strategy
justifyBiconditionalIntro knowledge goal =
  case goal of
    Biconditional lhs rhs ->
      (do
        block1 <- LList.fromFoldable (blocks knowledge)
        block2 <- LList.fromFoldable (blocks knowledge)
        pure $ block1 /\ block2
      )
      # findMap \(blockA /\ blockB) ->
        let valid = and
              [ (Proof.assumptions blockA # map _.formula) == [lhs]
              , (Proof.conclusion blockA # map _.formula) == Just rhs
              , (Proof.assumptions blockB # map _.formula) == [rhs]
              , (Proof.conclusion blockB # map _.formula) == Just lhs
              ]
         in guard valid (Just $ "↔I:" <> rangeOf blockA <> "," <> rangeOf blockB)
    _ -> Nothing

justifyBiconditionalElim :: Strategy
justifyBiconditionalElim knowledge goal =

      pretty <$> find valid candidates

  where

    biconditionals = knowledge # Array.mapMaybe \known -> case known of
        ProofLine line -> case line.formula of
          Biconditional lhs rhs -> Just $ known /\ lhs /\ rhs
          _ -> Nothing
        _ -> Nothing

    candidates = do
      bi /\ lhs /\ rhs <- LList.fromFoldable biconditionals
      st <- LList.fromFoldable (statements knowledge)
      pure $ bi /\ lhs /\ rhs /\ st

    valid (_ /\ lhs /\ rhs /\ statement) =
         statement.formula == lhs && goal == rhs
      || statement.formula == rhs && goal == lhs

    pretty (biconditional /\ _ /\ _ /\ statement) =
      "↔E:" <> rangeOf biconditional <> "," <> rangeOf (ProofLine statement)

justifyBottomIntro :: Strategy
justifyBottomIntro knowledge _goal =
  knowledge # Array.findMap \known -> case known of
    ProofLine line -> case line.formula of
      Conjunction lhs rhs ->
        guard (lhs == Negation rhs || rhs == Negation lhs)
              (Just $ "⊥I:" <> rangeOf known)
      _ -> Nothing
    _ -> Nothing

justifyNegationIntro :: Strategy
justifyNegationIntro knowledge goal =
  case goal of
    Negation negated ->
      blocks knowledge # Array.findMap \block ->
          let valid = (Proof.assumptions block <#> _.formula) == [negated]
                      && (Proof.conclusion block <#> _.formula) == Just Bottom
          in guard valid (Just $ "¬I:" <> rangeOf block)
    _ -> Nothing

justifyNegationElim :: Strategy
justifyNegationElim knowledge goal =
  knowledge # Array.findMap \known ->
    case known of
      ProofLine line -> case line.formula of
        Negation (Negation body) -> guard (body == goal) (Just $ "¬E:" <> rangeOf known)
        _ -> Nothing
      _ -> Nothing

justifyForallIntro :: Strategy
justifyForallIntro knowledge goal =
  case goal of
    Forall forallName forallClaim ->
      blocks knowledge
      # findMap (\block ->
        let assumptions = _.formula <$> Proof.assumptions block
            maybeConclusion = _.formula <$> Proof.conclusion block
        in case assumptions /\ maybeConclusion of
          [Declaration blockDeclaringName] /\ Just conclusion ->
            if Formula.substitute (Name_Obj_Fitch blockDeclaringName) (Name_Obj_FOL forallName) conclusion == forallClaim
            then Just ("∀I:" <> rangeOf block <> "[" <> getName blockDeclaringName <> "→" <> getName forallName <> "]")
            else Nothing
          _ -> Nothing)
    _ -> Nothing

justifyForallElim :: Strategy
justifyForallElim knowledge goal =

      pretty <$> find valid candidates

  where

    foralls = knowledge # Array.mapMaybe (\known ->
      case known of
        ProofLine line -> case line.formula of
          Forall forallName forallClaim -> Just $ known /\ forallName /\ forallClaim
          _ -> Nothing
        _ -> Nothing)

    candidates = do
      forall' /\ forallName /\ forallClaim <- LList.fromFoldable foralls
      freeVar <- LList.fromFoldable (Formula.freeVars goal)
      pure $ forall' /\ forallName /\ forallClaim /\ freeVar

    valid (_ /\ forallName /\ forallClaim /\ freeVar) =
      goal == (forallClaim # Formula.substitute (Name_Obj_FOL forallName) (Name_Obj_Fitch freeVar))

    pretty (forall' /\ forallName /\ _ /\ freeVar) =
      "∀E:" <> rangeOf forall' <> "[" <> getName forallName <> "→" <> getName freeVar <> "]"

justifyExistsIntro :: Strategy
justifyExistsIntro knowledge goal =
  case goal of
    Exists existsName existsClaim ->
      statements knowledge # findMap \statement ->
           let formula = statement.formula in
           Formula.freeVars formula # findMap \freeVar ->
            guard ((existsClaim # Formula.substitute (Name_Obj_FOL existsName) (Name_Obj_Fitch freeVar)) == formula)
                  (Just <<< fold $ [ "∃I:", rangeOf (ProofLine statement)
                                   , "[" <> getName freeVar <> "→" <> getName existsName <> "]"
                                   ])
    _ -> Nothing

justifyExistsElim :: Strategy
justifyExistsElim knowledge goal =
  (do
    st <- LList.fromFoldable (statements knowledge)
    bl <- LList.fromFoldable (blocks knowledge)
    pure $ st /\ bl
  )
  # findMap (\(statement /\ block) ->
    case statement.formula of
      Exists existsName existsClaim ->
        blockDeclarationNameAndAssumption block >>= \(blockDeclaringName /\ blockAssumption) ->
          let valid = and
                [ (existsClaim # Formula.substitute (Name_Obj_FOL existsName) (Name_Obj_Fitch blockDeclaringName)) == blockAssumption
                , (Proof.conclusion block <#> _.formula) == Just goal
                ]
          in guard valid (Just $ "∃E:" <> rangeOf (ProofLine statement) <> "," <> rangeOf block)
      _ -> Nothing)

  where

    -- ↓ Given a block proof where:
    -- ↓ * There are exactly 1 or 2 assumptions, and
    -- ↓ * The first assumption is a declaration,
    -- ↓ evaluates to (the name declared, the second assumption or Empty if there is none)
    blockDeclarationNameAndAssumption :: Proofy DecoratedLine -> Maybe (Name_Fitch /\ Formula)
    blockDeclarationNameAndAssumption block =
      case Proof.assumptions block <#> _.formula of
        [Declaration name, assumption] -> Just $ name /\ assumption
        [Declaration name] -> Just $ name /\ Empty
        _ -> Nothing

justifyEqualityIntro :: Strategy
justifyEqualityIntro _ = case _ of
  Equality lhs rhs -> guard (lhs == rhs) (Just "=I")
  _ -> Nothing

justifyEqualityElim :: Strategy
justifyEqualityElim knowledge goal =

    findMap mapper candidates

  where

    eqs = knowledge # Array.mapMaybe (\known -> case known of
      ProofLine line -> case line.formula of
        Equality lhs rhs -> Just $ known /\ lhs /\ rhs
        _ -> Nothing
      _ -> Nothing)

    candidates = do
      eq /\ lhs /\ rhs <- LList.fromFoldable eqs
      st <- LList.fromFoldable (statements knowledge)
      pure $ eq /\ lhs /\ rhs /\ st

    mapper (equality /\ lhs /\ rhs /\ statement) =
      let tryReplacement fromName toName =
            let valid = (statement.formula # Formula.substitute fromName toName) == goal
            in guard valid $ Just <<< fold $
                  [ "=E:", rangeOf equality, ",", rangeOf (ProofLine statement)
                  , "[", getName fromName, "→", getName toName, "]"
                  ]
      in tryReplacement lhs rhs # MaybeUtil.orElseLazy (\_ -> tryReplacement rhs lhs)

