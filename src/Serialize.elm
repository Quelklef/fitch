module Serialize exposing (..)

import ListUtil

import Parse exposing (..)
import Types exposing (Proofy(..))

escape : String -> String
escape string =
  string
  |> String.toList
  |> List.map (\char -> case char of
    '{' -> "\\{"
    '}' -> "\\}"
    ';' -> "\\;"
    ':' -> "\\:"
    '\\' -> "\\\\"
    c -> String.fromChar c)
  |> String.join ""

serialize : Proofy String -> String
serialize proof = case proof of
  ProofLine line -> escape line
  ProofBlock head body ->
    let serializedHead = List.map escape head |> List.map (\str -> str ++ ";") |> String.join ""
        serializedBody = List.map serialize body |> List.map (\str -> str ++ ";") |> String.join ""
    in "{" ++ serializedHead ++ ":" ++ serializedBody ++ "}"

--

deserialize : String -> Maybe (Proofy String)
deserialize =
  String.toList
  >> with parseProof (\result -> eof |> kThen (return result))
  >> Maybe.map Tuple.first

parseLine : Parser Char String
parseLine =
  withM (peek 2) <| \seen ->
    let first = ListUtil.get 0 seen
        second = ListUtil.get 1 seen
    in case first of
      Nothing -> Nothing
      Just '{' -> Just <| return ""
      Just '}' -> Just <| return ""
      Just ':' -> Just <| return ""
      Just ';' -> Just <| return ""
      Just '\\' -> second |> Maybe.map (\char -> drop 2 |> kThen (with parseLine <| \tail -> return (String.cons char tail)))
      Just char -> Just (drop 1 |> kThen (with parseLine <| \tail -> return (String.cons char tail)))

parseProof : Parser Char (Proofy String)
parseProof = parseBlock |> or (parseLine |> mapResults ProofLine)

semicolonTerminated : Parser Char a -> Parser Char a
semicolonTerminated parser = with parser (\result -> literal [';'] |> kThen (return result))

parseBlock : Parser Char (Proofy String)
parseBlock =
  literal ['{']
  |> kThen (with (zeroPlus <| semicolonTerminated parseLine) <| \headElements ->
  literal [':']
  |> kThen (with (zeroPlus <| semicolonTerminated parseProof) <| \bodyElements ->
  literal ['}']
  |> kThen (return <| ProofBlock headElements bodyElements)))
