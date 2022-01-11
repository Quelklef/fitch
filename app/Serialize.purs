module Fitch.Serialize where

import Prelude
import Data.Map as Map
import Data.Array as Array
import Data.List as List
import Data.List (List)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe, fromMaybe)
import Data.String.CodePoints as String
import Data.String.CodePoints (CodePoint)
import Data.Foldable (intercalate, elem)
import Data.Either (hush)
import Control.Alternative (guard)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (choice, try, lookAhead)

import Fitch.Types (Proofy (..))
import Fitch.Formula as Formula
import Fitch.Util.Parsing (match, token, unlazy)


-- ↓ Convert strings into and out of a portable, URI-embeddable encoding
foreign import toPayload :: String -> String
foreign import fromPayload :: String -> String


serialize :: Proofy String -> String
serialize =

    map preserve >>> toString >>> toPayload

  where

  -- ↓ Canonicize strings by preferring unicode characters (e.g. ∀)
  --   over ascii alternatives (resp. V) in order to make the URL string
  --   more resistent to version changes
  preserve :: String -> String
  preserve = Formula.prettifyText true

  toString :: Proofy String -> String
  toString = case _ of
    ProofLine line -> escape line
    ProofBlock head body ->
      let serializedHead = head # map (escape >>> (_ <> ";")) # intercalate ""
          serializedBody = body # map (toString >>> (_ <> ";")) # intercalate ""
      in "{" <> serializedHead <> ":" <> serializedBody <> "}"

    where

    escape :: String -> String
    escape =
      String.toCodePointArray
      >>> map (\char -> Map.lookup char mapping # fromMaybe (String.singleton char))
      >>> intercalate ""

    mapping = Map.fromFoldable <<< map (mapFst String.codePointFromChar) $
      [ '{' /\ "\\{"
      , '}' /\ "\\}"
      , ';' /\ "\\;"
      , ':' /\ "\\:"
      , '\\' /\ "\\\\"
      ]

    mapFst f (a /\ b) = f a /\ b


deserialize :: String -> Maybe (Proofy String)
deserialize =

    fromPayload >>> fromString

  where

  fromString :: String -> Maybe (Proofy String)
  fromString str =
    let chars = List.fromFoldable <<< String.toCodePointArray $ str
    in hush $ runParser chars parseProof

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

  match' :: Char -> Parser (List CodePoint) CodePoint
  match' = match <<< String.codePointFromChar
