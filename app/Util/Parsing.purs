module Fitch.Util.Parsing where

import Prelude
import Data.List (List)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Either (Either (..))
import Control.Monad.State.Trans (get, put)
import Text.Parsing.Parser (Parser, ParseState (..), ParseError (..), fail, runParser, position, failWithPosition)
import Text.Parsing.Parser.Pos (Position, initialPos)
import Text.Parsing.Parser.Token (match, token) as Parser
import Text.Parsing.Parser.Combinators (choice, try)

-- Workaround for cyclic references
unlazy :: forall s a. Monoid s => (Unit -> Parser s a) -> Parser s a
unlazy mkParser = do
  state <- get
  let r = runParser mempty
            (do put state
                res <- mkParser unit
                state' <- get
                pure $ res /\ state')
  case r of
    Left (ParseError err pos) -> failWithPosition err pos
    Right (val /\ state') -> put state' $> val

token :: forall tok. Parser (List tok) tok
token = Parser.token (const initialPos)

match :: forall tok. Eq tok => tok -> Parser (List tok) tok
match = Parser.match (const initialPos)

eof :: forall a. Show a => Parser (List a) Unit
eof = choice
  [ token >>= \tok -> fail ("Expected eof, got: " <> show tok)
  , pure unit
  ]
