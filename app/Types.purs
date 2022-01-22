module Fitch.Types where

import Prelude
import Data.Array.NonEmpty as NE
import Data.Either (Either)
import Data.String.CodePoints (CodePoint)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Control.Lazy (defer)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Arbitrary (arbitrary) as QC
import Test.QuickCheck.Gen (sized, resize, arrayOf, arrayOf1, oneOf) as QC

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
derive instance Generic Message _
instance Show Message where show = genericShow

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
derive instance Generic Formula _
instance Show Formula where show x = genericShow x

-- ↓ Something in the shape of a proof,
-- ↓ but containing an unknown type representing proof lines
-- ↓ A "real proof" would be a `Proofy Formula`
data Proofy lineT
  -- ↓ A line
  = ProofLine lineT
  -- ↓ Assumptions and body
  | ProofBlock (Array lineT) (Array (Proofy lineT))

derive instance Eq lineT => Eq (Proofy lineT)
derive instance Generic (Proofy ln) _
instance Show ln => Show (Proofy ln) where show x = genericShow x
derive instance Functor Proofy

instance Foldable Proofy where
  foldl pf = foldlDefault pf
  foldr pf = foldrDefault pf

  foldMap f (ProofLine ln) = f ln
  foldMap f (ProofBlock hd bd) = foldMap f hd <> foldMap (foldMap f) bd

instance Arbitrary ln => Arbitrary (Proofy ln) where
  arbitrary = defer \_ ->
    QC.sized \maxDepth ->
      let genLine = ProofLine <$> QC.arbitrary
          genBlock = ProofBlock
                     <$> (NE.toArray <$> QC.arrayOf1 QC.arbitrary)  -- empty heads generally disallowed
                     <*> QC.arrayOf (QC.resize (maxDepth - 1) QC.arbitrary)
      in if maxDepth == 0 then genLine
         else QC.oneOf (NE.singleton genLine <> NE.singleton genBlock)

-- ↓ Path to a formula in a proof, as a list of indicies
-- ↓ A negative index is an index into a block head; positive into the body
-- ↓ Thus, at a single depth, a proof is indexed [ -1 .. -A, 0, .. B ]
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
  , formula :: Formula
  , path :: Path
  , lineno :: Lineno
  , knowledge :: KnowledgeBox
  , justification :: Either String String
  }
