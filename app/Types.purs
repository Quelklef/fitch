module Fitch.Types where

import Prelude
import Data.Array.NonEmpty as NE
import Data.Either (Either)
import Data.String.CodePoints (CodePoint)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.String.CodePoints (singleton) as String
import Control.Lazy (defer)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Arbitrary (arbitrary) as QC
import Test.QuickCheck.Gen (sized, resize, arrayOf, arrayOf1, oneOf) as QC

-- ↓ The program model
type Model =
  { proof :: Proofy String
  , showDebugInfo :: Boolean
  , strictNames :: Boolean
  }

model0 :: Proofy String -> Model
model0 proof =
  { proof
  , showDebugInfo: false
  , strictNames: true
  }

-- ↓ A program message
data Message
  = ToggleDebugMode
  | ToggleStrictNames
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

-- Order-0 variable; ie, reference to a predicate-proposition
-- For instance, the P in ∀xPxy
newtype Name_Pred = Name_Pred CodePoint

-- Order-1 variable; ie, bound variable in FOL
-- For instance, the x in ∀xPxy
newtype Name_FOL = Name_FOL CodePoint

-- Fitch-system-specific name variables
-- For instance, the y in ∀xPxy
newtype Name_Fitch = Name_Fitch CodePoint

-- Reference to an object, ie, either a variable or a name variable
data Name_Obj = Name_Obj_FOL Name_FOL | Name_Obj_Fitch Name_Fitch

-- ↓ A formula in a proof
data Formula
  = Empty
  | Bottom
  -- ↓ Introduces a new variable
  | Declaration Name_Fitch
  -- ↓ Application of a predicate to 0 or more arguments
  -- ↓ (Propositions are treated as arity-0 predicates)
  | Application Name_Pred (Array Name_Obj)
  | Negation Formula
  | Conjunction Formula Formula
  | Disjunction Formula Formula
  | Implication Formula Formula
  | Biconditional Formula Formula
  | Forall Name_FOL Formula
  | Exists Name_FOL Formula
  | Equality Name_Obj Name_Obj

derive instance Eq Name_Pred
derive instance Generic Name_Pred _
instance Show Name_Pred where show x = genericShow x
instance Ord Name_Pred where compare (Name_Pred a) (Name_Pred b) = compare a b

derive instance Eq Name_FOL
derive instance Generic Name_FOL _
instance Show Name_FOL where show x = genericShow x
instance Ord Name_FOL where compare (Name_FOL a) (Name_FOL b) = compare a b

derive instance Eq Name_Fitch
derive instance Generic Name_Fitch _
instance Show Name_Fitch where show x = genericShow x
instance Ord Name_Fitch where compare (Name_Fitch a) (Name_Fitch b) = compare a b

derive instance Eq Name_Obj
derive instance Generic Name_Obj _
instance Show Name_Obj where show x = genericShow x

derive instance Eq Formula
derive instance Generic Formula _
instance Show Formula where show x = genericShow x

-- IsName ≅ Name_Pred + Name_FOL + Name_Fitch + Name_Obj
class IsName n where
  getName :: n -> String

instance IsName Name_Pred where
  getName (Name_Pred n) = String.singleton n

instance IsName Name_FOL where
  getName (Name_FOL n) = String.singleton n

instance IsName Name_Fitch where
  getName (Name_Fitch n) = String.singleton n

instance IsName Name_Obj where
  getName (Name_Obj_FOL n) = getName n
  getName (Name_Obj_Fitch n) = getName n


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
