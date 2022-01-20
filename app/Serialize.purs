module Fitch.Serialize where

import Prelude
import Data.Map as Map
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits as String
import Data.Foldable (intercalate)
import Data.Either (Either)
import Data.Bifunctor (lmap)
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.CodeUnits (oneOf, char, anyChar, eof)
import Text.Parsing.StringParser.Combinators (choice, lookAhead)

import Fitch.Types (Proofy (..))


-- ↓ Convert strings into and out of a portable, URI-embeddable encoding
foreign import toPayload :: String -> String
foreign import fromPayload :: String -> String


serialize :: Proofy String -> String
serialize =

    toString >>> toPayload

  where

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
      String.toCharArray
      >>> map (\char -> Map.lookup char mapping # fromMaybe (String.singleton char))
      >>> intercalate ""

    mapping = Map.fromFoldable $
      [ '{' /\ "\\{"
      , '}' /\ "\\}"
      , ';' /\ "\\;"
      , ':' /\ "\\:"
      , '\\' /\ "\\\\"
      ]


deserialize :: String -> Either String (Proofy String)
deserialize =

    fromPayload >>> fromString

  where

  fromString :: String -> Either String (Proofy String)
  fromString str = lmap _.error $ runParser parseProof str

  parseLine :: Parser String
  parseLine = choice <<< map try $

    [ lookAhead' (void (oneOf ['{', '}', ':', ';']) <|> eof) *> pure ""

    , do void (char '\\')
         second <- anyChar
         tail <- parseLine
         pure $ String.singleton second <> tail

    , do first <- anyChar
         tail <- parseLine
         pure $ String.singleton first <> tail

    ]

    where lookAhead' = try <<< lookAhead
          -- https://github.com/purescript-contrib/purescript-string-parsers/issues/73

  parseProof :: Parser (Proofy String)
  parseProof = choice <<< map try $
    [ defer \_ -> parseBlock
    , ProofLine <$> parseLine
    ]

  parseBlock :: Parser (Proofy String)
  parseBlock = do
    void $ char '{'
    headElems <- Array.many (try $ parseLine <* char ';')
    void $ char ':'
    bodyElems <- Array.many (try $ parseProof <* char ';')
    void $ char '}'
    pure $ ProofBlock headElems bodyElems
