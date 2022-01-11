module Fitch.Serialize where

import Prelude
import Data.Map as Map
import Data.Array as Array
import Data.List as List
import Data.List (List)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe (..))
import Data.String.CodePoints as String
import Data.String.CodePoints (CodePoint)
import Data.Foldable (intercalate, elem)
import Data.Either (hush)
import Control.Alternative (guard)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (choice, try, lookAhead)

import Fitch.Types (Proofy (..))
import Fitch.Util.Parsing (match, token, unlazy)

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
    let serializedHead = head # map (escape >>> (_ <> ";")) # intercalate ""
        serializedBody = body # map (serialize >>> (_ <> ";")) # intercalate ""
    in "{" <> serializedHead <> ":" <> serializedBody <> "}"

--

match' :: Char -> Parser (List CodePoint) CodePoint
match' = match <<< String.codePointFromChar

deserialize :: String -> Maybe (Proofy String)
deserialize str = hush $ runParser chars parseProof
  where chars = List.fromFoldable <<< String.toCodePointArray $ str

parseLine :: Parser (List CodePoint) String
parseLine = choice <<< map try $

  [ do first <- lookAhead token
       guard $ first `elem` (String.codePointFromChar <$> ['{', '}', ':', ';'])
       pure ""

  , do void (match' '\\')
       second <- token
       tail <- parseLine
       pure $ String.singleton second <> tail

  , do first <- token
       tail <- parseLine
       pure $ String.singleton first <> tail

  ]

parseProof :: Parser (List CodePoint) (Proofy String)
parseProof = choice <<< map try $
  [ unlazy \_ -> parseBlock
  , ProofLine <$> parseLine
  ]

parseBlock :: Parser (List CodePoint) (Proofy String)
parseBlock = do
  void $ match' '{'
  headElems <- Array.many (try $ parseLine <* match' ';')
  void $ match' ':'
  bodyElems <- Array.many (try $ parseProof <* match' ';')
  void $ match' '}'
  pure $ ProofBlock headElems bodyElems
