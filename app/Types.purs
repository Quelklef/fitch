module Fitch.Types where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.String.CodePoints (CodePoint)

-- ↓ The program model
type Model =
  { proof :: Proofy String
  , showDebugInfo :: Boolean
  }

-- ↓ A program message
data Message
  = ToggleDebugMode
  | CopyProofToClipboard
  | SetProofTo (Proofy String)
  | Noop
  | SetFocusTo Path
  | SetFormulaAt Path String
  | NewLineAfter Path Boolean
  | IndentAt Path
  | DedentAt Path
  | BackspaceAt Path

derive instance Eq Message

-- ↓ A formula in a proof
data Formula
  = Empty
  | Bottom
  -- ↓ Introduces a new variable
  | Declaration CodePoint
  -- ↓ Application of a predicate to 0 or more arguments
  -- ↓ (Propositions are treated as arity-0 predicates)
  | Application CodePoint (Array CodePoint)
  | Negation Formula
  | Conjunction Formula Formula
  | Disjunction Formula Formula
  | Implication Formula Formula
  | Biconditional Formula Formula
  | Forall CodePoint Formula
  | Exists CodePoint Formula
  | Equality CodePoint CodePoint

derive instance Eq Formula

-- ↓ Something in the shape of a proof,
-- ↓ but containing an unknown type representing proof lines
-- ↓ A "real proof" would be a `Proofy Formula`
data Proofy lineT
  -- ↓ A line
  = ProofLine lineT
  -- ↓ Assumptions and body
  | ProofBlock (Array lineT) (Array (Proofy lineT))

derive instance Eq lineT => Eq (Proofy lineT)
derive instance Functor Proofy

-- ↓ Path to a formula in a proof, as a list of indicies
-- ↓ A negative index is an index into a block head; positive into the body
type Path = Array Int

-- ↓ A line number in a proof
type Lineno = Int

-- ↓ A 'context' or 'scope' of all the proofs available for a particular line to use in justification
type Knowledge = Array (Proofy DecoratedLine)

-- ↓ To fix a circular type alias issue
data KnowledgeBox = KnowledgeBox Knowledge

-- ↓ A proof line, decorated with a bunch of extra info
type DecoratedLine =
  { text :: String
  , formula :: Maybe Formula
  , path :: Path
  , lineno :: Lineno
  , knowledge :: KnowledgeBox
  , justification :: Either String String
  }
