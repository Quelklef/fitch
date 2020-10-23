module Decorate exposing (..)

import ListUtil
import StringUtil

import Types exposing (Proofy(..), Path, Formula, Lineno, Knowledge, KnowledgeBox(..), DecoratedLine)
import Path
import Proof
import Formula
import Semantics
import TextStyle

decorate : Proofy String -> Proofy DecoratedLine
decorate = decorate_ (1, [], []) >> Tuple.first

decorate_ : (Lineno, Path, Knowledge) -> Proofy String -> (Proofy DecoratedLine, Lineno)
decorate_ (lineno, path, knowledge) proof = case proof of
  ProofLine text ->
    let formula = Formula.parse text
        decorated =
          { text = text
          , formula = formula
          , path = path
          , lineno = lineno
          , knowledge = KnowledgeBox knowledge
          , justification =
              formula
              |> Result.fromMaybe "malformed"
              |> Result.andThen (\justFormula ->
              Semantics.verifySemantics knowledge justFormula
              |> Result.andThen (\() ->
              Semantics.justify knowledge justFormula
              |> Result.fromMaybe "invalid"))
          }
    in (ProofLine decorated, (lineno + 1))

  ProofBlock head body ->
    let (decoratedHead, lineno2, knowledge2) = decorateHead (0, lineno, (path, knowledge)) head
        (decoratedBody, lineno3) = decorateBody (0, lineno2, (path, knowledge2)) body
    in (ProofBlock decoratedHead decoratedBody, lineno3)

decorateHead : (Int, Lineno, (Path, Knowledge)) -> List String -> (List DecoratedLine, Lineno, Knowledge)
decorateHead (idx, lineno, (pathStub, knowledge)) lines = case lines of
  [] -> ([], lineno, knowledge)
  lineText::restLines ->
    let formula = Formula.parse lineText
        decoratedLine =
          { text = lineText
          , formula = formula
          , path = pathStub ++ [-idx-1]
          , lineno = lineno
          , knowledge = KnowledgeBox knowledge
          , justification =
              formula
              |> Result.fromMaybe "malformed"
              |> Result.andThen (Semantics.verifySemantics knowledge)
              |> Result.map (always "assumed")
          }
        (decoratedRest, lineno2, knowledge2) = restLines |> decorateHead (idx + 1, lineno + 1, (pathStub, knowledge ++ [ProofLine decoratedLine]))
    in (decoratedLine :: decoratedRest, lineno2, knowledge2)

decorateBody : (Int, Lineno, (Path, Knowledge)) -> List (Proofy String) -> (List (Proofy DecoratedLine), Lineno)
decorateBody (idx, lineno, (pathStub, knowledge)) proofs = case proofs of
  [] -> ([], lineno)
  first::rest ->
    let (decoratedFirst, lineno2) = decorate_ (lineno, pathStub ++ [idx], knowledge) first
        (decoratedRest, lineno3) = decorateBody (idx + 1, lineno2, (pathStub, knowledge ++ [decoratedFirst])) rest
    in (decoratedFirst :: decoratedRest, lineno3)

--

toText : Bool -> Proofy DecoratedLine -> String
toText useUnicode proof =
  toText_ useUnicode (maxLineLength useUnicode proof + 5) 0 proof
  |> TextStyle.map useUnicode

maxLineLength : Bool -> Proofy DecoratedLine -> Int
maxLineLength useUnicode proof =
  case proof of
    ProofLine line ->
      String.length " "
      + String.length (String.fromInt line.lineno)
      + String.length ". "
      + String.length (Formula.prettifyText useUnicode line.text)

    ProofBlock head body ->
      let headMaxs = List.map (maxLineLength useUnicode << ProofLine) head
          bodyMaxs = List.map (maxLineLength useUnicode) body
          max = List.append headMaxs bodyMaxs |> List.maximum |> Maybe.withDefault 0
      in max + 1

toText_ : Bool -> Int -> Int -> Proofy DecoratedLine -> String
toText_ useUnicode assumptionsColumn depth proof =
  case proof of
    ProofLine line ->
      let justnText = case line.justification of
            Ok text -> text
            Err text -> text
      in
        (String.repeat depth "│" ++ " " ++ String.fromInt line.lineno ++ ". " ++ Formula.prettifyText useUnicode line.text
        |> StringUtil.padTo ' ' assumptionsColumn)
        ++ Formula.prettifyText useUnicode justnText

    ProofBlock head body ->
      let headTexts = head |> List.map ProofLine |> List.map (toText_ useUnicode assumptionsColumn (depth + 1))
          bar = String.repeat depth "│" ++ "├──────────"
          bodyTexts =
            ListUtil.zip body ((List.drop 1 body |> List.map Just) ++ [Nothing])
            |> ListUtil.flatMap (\(thisBodyProof, nextBodyProof) ->
              let thisBodyProofIsBlock = case thisBodyProof of
                    ProofBlock _ _ -> True
                    _ -> False
                  nextBodyProofIsBlock = case nextBodyProof of
                    Just (ProofBlock _ _) -> True
                    _ -> False
              in [toText_ useUnicode assumptionsColumn (depth + 1) thisBodyProof]
                -- vv Include a separator between adjacent proofs
                 ++ ListUtil.if_ (thisBodyProofIsBlock && nextBodyProofIsBlock) (String.repeat (depth + 1) "│"))

      in String.join "\n" <| headTexts ++ [bar] ++ bodyTexts

