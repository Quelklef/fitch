module Fitch.Decorate where

import Prelude
import Data.Either (Either (..))
import Data.Either as Either
import Data.Monoid (power)
import Data.Array as Array
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (intercalate, maximum)
import Data.String.CodePoints as String

import Fitch.Types (Proofy(..), Path, Lineno, Knowledge, KnowledgeBox(..), DecoratedLine)
import Fitch.Formula as Formula
import Fitch.Semantics as Semantics
import Fitch.TextStyle as TextStyle
import Fitch.Util.ArrayUtil as ArrayUtil
import Fitch.Util.StringUtil as StringUtil

decorate :: Proofy String -> Proofy DecoratedLine
decorate = decorate_ (1 /\ [] /\ []) >>> fst

decorate_ :: (Lineno /\ Path /\ Knowledge) -> Proofy String -> (Proofy DecoratedLine /\ Lineno)
decorate_ (lineno /\ path /\ knowledge) proof = case proof of
  ProofLine text ->
    let formula = Formula.parse text
        decorated =
          { text: text
          , formula: formula
          , path: path
          , lineno: lineno
          , knowledge: KnowledgeBox knowledge
          , justification:
              formula
              # Either.note "malformed"
              >>= (\justFormula ->
              Semantics.verifySemantics knowledge justFormula
              >>= (\_ ->
              Semantics.justify knowledge justFormula
              # Either.note "invalid"))
          }
    in ProofLine decorated /\ (lineno + 1)

  ProofBlock head body ->
    let (decoratedHead /\ lineno2 /\ knowledge2) = decorateHead (0 /\ lineno /\ (path /\ knowledge)) head
        (decoratedBody /\ lineno3) = decorateBody (0 /\ lineno2 /\ (path /\ knowledge2)) body
    in ProofBlock decoratedHead decoratedBody /\ lineno3

decorateHead :: (Int /\ Lineno /\ (Path /\ Knowledge)) -> Array String -> (Array DecoratedLine /\ Lineno /\ Knowledge)
decorateHead (idx /\ lineno /\ (pathStub /\ knowledge)) = Array.uncons >>> case _ of
  Nothing -> [] /\ lineno /\ knowledge
  Just { head: lineText, tail: restLines } ->
    let formula = Formula.parse lineText
        decoratedLine =
          { text: lineText
          , formula: formula
          , path: pathStub <> [-idx-1]
          , lineno: lineno
          , knowledge: KnowledgeBox knowledge
          , justification:
              formula
              # Either.note "malformed"
              >>= (Semantics.verifySemantics knowledge)
              # map (const "assumed")
          }
        decoratedRest /\ lineno2 /\ knowledge2 = restLines # decorateHead ((idx + 1) /\ (lineno + 1) /\ (pathStub /\ (knowledge <> [ProofLine decoratedLine])))
    in Array.cons decoratedLine decoratedRest /\ lineno2 /\ knowledge2

decorateBody :: Int /\ Lineno /\ (Path /\ Knowledge) -> Array (Proofy String) -> Array (Proofy DecoratedLine) /\ Lineno
decorateBody (idx /\ lineno /\ (pathStub /\ knowledge)) = Array.uncons >>> case _ of
  Nothing -> [] /\ lineno
  Just { head: first, tail: rest } ->
    let decoratedFirst /\ lineno2 = decorate_ (lineno /\ (pathStub <> [idx]) /\ knowledge) first
        decoratedRest /\ lineno3 = decorateBody ((idx + 1) /\ lineno2 /\ (pathStub /\ (knowledge <> [decoratedFirst]))) rest
    in Array.cons decoratedFirst decoratedRest /\ lineno3

--

toText :: Boolean -> Proofy DecoratedLine -> String
toText useUnicode proof =
  toText_ useUnicode (maxLineLength useUnicode proof + 5) 0 proof
  # TextStyle.map useUnicode

maxLineLength :: Boolean -> Proofy DecoratedLine -> Int
maxLineLength useUnicode proof =
  case proof of
    ProofLine line ->
      String.length " "
      + String.length (show line.lineno)
      + String.length ". "
      + String.length (Formula.prettifyText useUnicode line.text)

    ProofBlock head body ->
      let headMaxs = (maxLineLength useUnicode <<< ProofLine) <$> head
          bodyMaxs = maxLineLength useUnicode <$> body
          max = (headMaxs <> bodyMaxs) # maximum # fromMaybe 0
      in max + 1

toText_ :: Boolean -> Int -> Int -> Proofy DecoratedLine -> String
toText_ useUnicode assumptionsColumn depth proof =
  case proof of
    ProofLine line ->
      let justnText = case line.justification of
            Right text -> text
            Left text -> text
      in
        ("│" `power` depth <> " " <> show line.lineno <> ". " <> Formula.prettifyText useUnicode line.text
        # StringUtil.padTo (String.codePointFromChar ' ') assumptionsColumn)
        <> Formula.prettifyText useUnicode justnText

    ProofBlock head body ->
      let headTexts = head <#> ProofLine <#> toText_ useUnicode assumptionsColumn (depth + 1)
          bar = "│" `power` depth <> "├──────────"
          bodyTexts =
            ArrayUtil.zip body ((Array.drop 1 body <#> Just) <> [Nothing])
            # ArrayUtil.flatMap (\(thisBodyProof /\ nextBodyProof) ->
              let thisBodyProofIsBlock = case thisBodyProof of
                    ProofBlock _ _ -> true
                    _ -> false
                  nextBodyProofIsBlock = case nextBodyProof of
                    Just (ProofBlock _ _) -> true
                    _ -> false
              in [toText_ useUnicode assumptionsColumn (depth + 1) thisBodyProof]
                -- ↓ Include a separator between adjacent proofs
                 <> ArrayUtil.if_ (thisBodyProofIsBlock && nextBodyProofIsBlock) ("│" `power` (depth + 1)))

      in intercalate "\n" $ headTexts <> [bar] <> bodyTexts

