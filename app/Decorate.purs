module Fitch.Decorate where

import Prelude
import Data.Either (Either (..))
import Data.Either as Either
import Data.Monoid (guard, power)
import Data.Array as Array
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (intercalate, maximum, sum)
import Data.String.CodePoints as String

import Fitch.Types (Proofy(..), Path, Lineno, Knowledge, KnowledgeBox(..), DecoratedLine)
import Fitch.Formula as Formula
import Fitch.Semantics as Semantics
import Fitch.Util.StringUtil as StringUtil

decorate :: Proofy String -> Proofy DecoratedLine
decorate = decorateImpl 1 [] [] >>> fst

  where

  decorateImpl :: Lineno -> Path -> Knowledge -> Proofy String -> Proofy DecoratedLine /\ Lineno
  decorateImpl lineno path knowledge = case _ of
    ProofLine text ->
      let mayFormula = Formula.parse text
          decorated =
            { text
            , formula: mayFormula
            , path
            , lineno
            , knowledge: KnowledgeBox knowledge
            , justification: do
                formula <- Either.note "malformed" mayFormula
                Semantics.verifySemantics knowledge formula
                Semantics.justify knowledge formula # Either.note "invalid"
            }
      in ProofLine decorated /\ (lineno + 1)

    ProofBlock head body ->
      let decoratedHead /\ lineno2 /\ knowledge2 = decorateHead 0 lineno path knowledge head
          decoratedBody /\ lineno3 = decorateBody 0 lineno2 path knowledge2 body
      in ProofBlock decoratedHead decoratedBody /\ lineno3

  decorateHead :: Int -> Lineno -> Path -> Knowledge -> Array String -> Array DecoratedLine /\ Lineno /\ Knowledge
  decorateHead idx lineno pathStub knowledge = Array.uncons >>> case _ of
    Nothing -> [] /\ lineno /\ knowledge
    Just { head: lineText, tail: restLines } ->
      let mayFormula = Formula.parse lineText
          decoratedLine =
            { text: lineText
            , formula: mayFormula
            , path: pathStub <> [-idx-1]
            , lineno: lineno
            , knowledge: KnowledgeBox knowledge
            , justification: do
                formula <- Either.note "malformed" mayFormula
                Semantics.verifySemantics knowledge formula
                pure "assumed"
            }
          decoratedRest /\ lineno2 /\ knowledge2 = restLines # decorateHead (idx + 1) (lineno + 1) pathStub (knowledge <> [ProofLine decoratedLine])
      in Array.cons decoratedLine decoratedRest /\ lineno2 /\ knowledge2

  decorateBody :: Int -> Lineno -> Path -> Knowledge -> Array (Proofy String) -> Array (Proofy DecoratedLine) /\ Lineno
  decorateBody idx lineno pathStub knowledge = Array.uncons >>> case _ of
    Nothing -> [] /\ lineno
    Just { head: first, tail: rest } ->
      let decoratedFirst /\ lineno2 = decorateImpl lineno (pathStub <> [idx]) knowledge first
          decoratedRest /\ lineno3 = decorateBody (idx + 1) lineno2 pathStub (knowledge <> [decoratedFirst]) rest
      in Array.cons decoratedFirst decoratedRest /\ lineno3


toText :: Proofy DecoratedLine -> String
toText proof =

    toTextImpl (maxLineLength proof + 5) 0 proof

  where

  maxLineLength :: Proofy DecoratedLine -> Int
  maxLineLength =
    case _ of
      ProofLine line ->
        sum <<< map String.length
        $ [ " ", show line.lineno, ". ", Formula.prettifyText line.text ]

      ProofBlock head body ->
        let headMaxs = (maxLineLength <<< ProofLine) <$> head
            bodyMaxs = maxLineLength <$> body
            max = (headMaxs <> bodyMaxs) # (maximum >>> fromMaybe 0)
        in max + 1

  toTextImpl :: Int -> Int -> Proofy DecoratedLine -> String
  toTextImpl assumptionsColumn depth =
    case _ of
      ProofLine line ->
        (<>)
          ("│" `power` depth <> " " <> show line.lineno <> ". " <> Formula.prettifyText line.text
            # StringUtil.padTo (String.codePointFromChar ' ') assumptionsColumn)
          (Formula.prettifyText $ getJustificationText line)

      ProofBlock head body ->
        let headTexts = head <#> ProofLine <#> toTextImpl assumptionsColumn (depth + 1)
            bar = "│" `power` depth <> "├──────────"
            bodyTexts =
              Array.zip body ((Array.drop 1 body <#> Just) <> [Nothing])
              >>= (\(thisBodyProof /\ nextBodyProof) ->
                   [toTextImpl assumptionsColumn (depth + 1) thisBodyProof]
                   -- ↓ Include a separator between adjacent proofs
                   <> guard (isBlock thisBodyProof && isBlock' nextBodyProof) ["│" `power` (depth + 1)])

        in intercalate "\n" $ headTexts <> [bar] <> bodyTexts

  isBlock :: forall a. Proofy a -> Boolean
  isBlock = case _ of
    ProofBlock _ _ -> true
    _ -> false

  isBlock' :: forall a. Maybe (Proofy a) -> Boolean
  isBlock' = case _ of
    Just (ProofBlock _ _) -> true
    _ -> false

  getJustificationText :: DecoratedLine -> String
  getJustificationText = _.justification >>> case _ of
    Right text -> text
    Left text -> text
