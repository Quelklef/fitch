module Fitch.Serialize where

import Prelude
import Data.Map as Map
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe (..))
import Data.String.CodePoints as String
import Data.String.CodePoints (CodePoint)
import Data.Tuple (fst)
import Data.Foldable (intercalate, elem)

import Fitch.Types (Proofy (..))
import Fitch.Util.Parse
import Fitch.Util.ArrayUtil as ArrayUtil

escape :: String -> String
escape string =
  string
  # String.toCodePointArray
  # map (\char ->
      case Map.lookup char mapping of
        Just str -> str
        Nothing -> String.singleton char)
  # intercalate ""

  where
  mapping = Map.fromFoldable $
    [ String.codePointFromChar '{' /\ "\\{"
    , String.codePointFromChar '}' /\ "\\}"
    , String.codePointFromChar ';' /\ "\\;"
    , String.codePointFromChar ':' /\ "\\:"
    , String.codePointFromChar '\\' /\ "\\\\"
    ]

serialize :: Proofy String -> String
serialize proof = case proof of
  ProofLine line -> escape line
  ProofBlock head body ->
    let serializedHead = map escape head # map (\str -> str <> ";") # intercalate ""
        serializedBody = map serialize body # map (\str -> str <> ";") # intercalate ""
    in "{" <> serializedHead <> ":" <> serializedBody <> "}"

--

deserialize :: String -> Maybe (Proofy String)
deserialize =
  String.toCodePointArray
  >>> with parseProof (\result -> eof # kThen (return result))
  >>> map fst

parseLine :: Parser CodePoint String
parseLine =
  withM (peek 2) $ \seen ->
    let first = Array.index seen 0
        second = Array.index seen 1
    in case first of
      Nothing -> Nothing
      Just char ->
        if char `elem` (String.codePointFromChar <$> ['{', '}', ':', ';'])
          then Just (return "")
        else if char == String.codePointFromChar '\\'
          then second <#> (\char -> drop 2 # kThen (with parseLine $ \tail -> return (String.singleton char <> tail)))
        else
          Just (drop 1 # kThen (with parseLine $ \tail -> return (String.singleton char <> tail)))

parseProof :: Parser CodePoint (Proofy String)
parseProof = (\t -> parseBlock t) # or (parseLine # mapResults ProofLine)

semicolonTerminated :: forall a. Parser CodePoint a -> Parser CodePoint a
semicolonTerminated parser = with parser (\result -> literal [String.codePointFromChar ';'] # kThen (return result))

parseBlock :: Parser CodePoint (Proofy String)
parseBlock =
  literal [String.codePointFromChar '{']
  # kThen (with (zeroPlus $ semicolonTerminated parseLine) $ \headElements ->
  literal [String.codePointFromChar ':']
  # kThen (with (zeroPlus $ semicolonTerminated parseProof) $ \bodyElements ->
  literal [String.codePointFromChar '}']
  # kThen (return $ ProofBlock headElements bodyElements)))
