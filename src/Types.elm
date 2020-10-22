module Types exposing (..)

-- vv The program model
type alias Model =
  { proof : Proofy String
  , showDebugInfo : Bool
  , useUnicode : Bool
  }

-- vv A program message
type Message
  = ToggleDebugMode
  | ToggleUseUnicode
  | SetProofTo (Proofy String)
  | Noop
  | SetFocusTo Path
  | SetFormulaAt Path String
  | NewLineAfter Path Bool
  | IndentAt Path
  | DedentAt Path
  | BackspaceAt Path

-- vv A formula in a proof
type Formula
  = Empty
  | Bottom
  -- vv Reference to a variable or proposition
  | Name Char
  -- vv Introduces a new variable
  | Declaration Char
  -- vv Application of a predicate to 0 or more arguments
  -- vv (Propositions are treated as arity-0 predicates)
  | Application Char (List Char)
  | Negation Formula
  | Conjunction Formula Formula
  | Disjunction Formula Formula
  | Implication Formula Formula
  | Biconditional Formula Formula
  | Forall Char Formula
  | Exists Char Formula
  | Equality Char Char

-- vv Something in the shape of a proof,
-- vv but containing an unknown type representing proof lines
-- vv A "real proof" would be a `Proofy Formula`
type Proofy lineT
  -- vv A line
  = ProofLine lineT
  -- vv Assumptions and body
  | ProofBlock (List lineT) (List (Proofy lineT))

-- vv Path to a formula in a proof, as a list of indicies
-- vv A negative index is an index into a block head; positive into the body
type alias Path = List Int

-- vv A line number in a proof
type alias Lineno = Int

-- vv A 'context' or 'scope' of all the proofs available for a particular line to use in justification
type alias Knowledge = List (Proofy DecoratedLine)

-- vv To fix a circular type aliasing issue
type KnowledgeBox = KnowledgeBox Knowledge

-- vv A proof line, decorated with a bunch of extra info
type alias DecoratedLine =
  { text : String
  , formula : Maybe Formula
  , path : Path
  , lineno : Lineno
  , knowledge : KnowledgeBox
  , justification : Result String String
  }
