module Fitch.Formula where

import Prelude
import Data.Array as Array
import Data.Either (Either (..), hush)
import Data.Set as Set
import Data.Set (Set)
import Data.Maybe (Maybe (..))
import Data.Tuple.Nested ((/\))
import Data.Foldable (fold)
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints (drop, length, singleton, take, stripPrefix, codePointAt) as String
import Data.String.Pattern (Pattern (..)) as String
import Data.String.Utils (filter) as String
import Data.String.CodeUnits as StringU
import Data.CodePoint.Unicode (isAlpha, isLower, isUpper)
import Control.Lazy (defer)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser (..), fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (eof, string)
import Text.Parsing.StringParser.CodeUnits (anyChar)
import Text.Parsing.StringParser.Combinators (choice, many)

import Fitch.Types (Formula (..), Name_Pred (..), Name_FOL (..), Name_Fitch (..), Name_Obj (..), getName)


desugar :: String -> String
desugar = case _ of
  "" -> ""
  str ->
    let pref /\ suff =
          mapping
          # Array.findMap (\(key /\ val) ->
              (val /\ _) <$> String.stripPrefix (String.Pattern key) str)
          # case _ of
              Just x -> x
              Nothing -> String.take 1 str /\ String.drop 1 str

    in pref <> desugar suff

  where

  mapping =
    [ ">"  /\ "→"
    , "<>" /\ "↔"
    , "_"  /\ "⊥"
    , "#"  /\ "⊥"
    , "-"  /\ "¬"
    , "~"  /\ "¬"
    , "!"  /\ "¬"
    , "&"  /\ "∧"
    , "*"  /\ "∧"
    , "."  /\ "∧"
    , "|"  /\ "∨"
    , "v"  /\ "∨"
    , "+"  /\ "∨"
    , "∀"  /\ "∀"
    , "\\" /\ "∀"
    , "V"  /\ "∀"
    , "@"  /\ "∃"
    , "E"  /\ "∃"
    , "!=" /\ "≠"
    , "/=" /\ "≠"
    ]

type Scope = Set Name_FOL

parse :: String -> Maybe Formula
parse =

    \str -> hush $ runParser (parseEmpty <|> parseFormula mempty <* eof) (String.filter (_ /= " ") str)

  where

  anyCodePoint :: Parser CodePoint
  anyCodePoint = Parser \{ str, pos } ->
    case String.codePointAt 0 (StringU.drop pos str) of
      Just cp -> Right { result: cp, suffix: { str, pos: pos + String.length (String.singleton cp) } }
      Nothing -> Left { pos, error: "Unexpected EOF" }

  parseBottom :: Parser Formula
  parseBottom = string "⊥" $> Bottom

  parseTaut :: Parser Formula
  parseTaut = void (string "⊤") $> Empty

  parseEmpty :: Parser Formula
  parseEmpty = eof $> Empty

  parseDeclaration :: Parser Formula
  parseDeclaration = do
    void $ string "["
    char <- anyCodePoint
    void $ string "]"

    -- ↓ As a special case, allow comments after declarations
    void $ many anyChar

    if isAlpha char
    then pure (Declaration (Name_Fitch char))
    else fail "Expected declaration"

  parseNameRaw :: Parser CodePoint
  parseNameRaw = do
    char <- anyCodePoint
    if isAlpha char
    then pure char
    else fail "Expected name"

  parsePredName :: Parser Name_Pred
  parsePredName = do
    char <- parseNameRaw
    if isUpper char
    then pure $ Name_Pred char
    else fail "Predicates must be uppercase"

  parseObjName :: Scope -> Parser Name_Obj
  parseObjName sc = do
    char <- parseNameRaw
    when (not $ isLower char) $
      fail "Variables must be lowercase"
    pure $
      -- ↓ Infer free variables to be name variables
      if Name_FOL char `Set.member` sc
      then Name_Obj_FOL (Name_FOL char)
      else Name_Obj_Fitch (Name_Fitch char)

  parseApplication :: Scope -> Parser Formula
  parseApplication sc =
    choice <<< map try $
      [ do pred <- parsePredName
           vals <- Array.many (try $ parseObjName sc)
           pure $ Application pred vals

      -- Allow aRb to mean Rab
      , do a <- parseObjName sc
           r <- parsePredName
           b <- parseObjName sc
           pure $ Application r [a, b]
      ]

  parseNegation :: Scope -> Parser Formula
  parseNegation sc = defer \_ -> string "¬" *> (Negation <$> parseNonBinOp sc)

  parseParenthesized :: Scope -> Parser Formula
  parseParenthesized sc = defer \_ -> string "(" *> parseFormula sc <* string ")"

  parseBinOp ::
    forall lhs rhs res
    .  String
    -> Parser lhs
    -> Parser rhs
    -> (lhs -> rhs -> res)
    -> Parser res
  parseBinOp opToken lhsParser rhsParser makeResult = do
    lhs <- lhsParser
    void $ string opToken
    rhs <- rhsParser
    pure $ makeResult lhs rhs

  parseEquality sc =
    parseBinOp
    "="
    (parseObjName sc)
    (parseObjName sc)
    Equality

  parseInequality sc =
    parseBinOp
    "≠"
    (parseObjName sc)
    (parseObjName sc)
    (\lhs rhs -> Negation (Equality lhs rhs))

  parseForall :: Scope -> Parser Formula
  parseForall = defer \_ -> parseBinding "∀" Forall

  parseExists :: Scope -> Parser Formula
  parseExists = defer \_ -> parseBinding "∃" Exists

  parseBinding
    :: String -> (Name_FOL -> Formula -> Formula)
    -> Scope -> Parser Formula
  parseBinding symb mk sc = do
    void $ string symb
    char <- parseNameRaw
    when (not $ isLower char) (fail "Variables must be lowercase")
    let name = Name_FOL char
    let sc' = Set.insert name sc
    body <- parseEmpty <|> parseNonBinOp sc'
    pure $ mk name body

  parseNonBinOp :: Scope -> Parser Formula
  parseNonBinOp sc = defer \_ ->
    choice <<< map try $
      [ parseTaut
      , parseBottom
      , parseNegation sc
      , parseEquality sc
      , parseInequality sc
      , parseParenthesized sc
      , parseApplication sc
      , parseDeclaration
      , parseForall sc
      , parseExists sc
      ]

  parseBinOpWithFallthrough ::
    forall res
    .  String
    -> Parser res
    -> (res -> res -> res)
    -> Parser res
  parseBinOpWithFallthrough op innerParser makeResult = do
    val <- innerParser
    choice <<< map try $
      [ do let lhs = val
           void $ string op
           rhs <- innerParser
           pure $ makeResult lhs rhs
      , do pure val
      ]

  parseConjunction sc = defer \_ -> parseBinOpWithFallthrough "∧" (parseNonBinOp sc) Conjunction
  parseDisjunction sc = defer \_ -> parseBinOpWithFallthrough "∨" (parseConjunction sc) Disjunction
  parseImplication sc = defer \_ -> parseBinOpWithFallthrough "→" (parseDisjunction sc) Implication
  parseBiconditional sc = defer \_ -> parseBinOpWithFallthrough "↔" (parseImplication sc) Biconditional

  parseFormula :: Scope -> Parser Formula
  parseFormula sc = defer \_ -> parseBiconditional sc


pretty :: Formula -> String
pretty = case _ of
  Empty -> "⊤"
  Bottom -> "⊥"
  Declaration name -> "[" <> getName name <> "]"
  Application name args -> getName name <> fold (getName <$> args)
  Negation body -> "¬(" <> pretty body <> ")"
  Conjunction lhs rhs -> "(" <> pretty lhs <> ")∧(" <> pretty rhs <> ")"
  Disjunction lhs rhs -> "(" <> pretty lhs <> ")∨(" <> pretty rhs <> ")"
  Implication lhs rhs -> "(" <> pretty lhs <> ")→(" <> pretty rhs <> ")"
  Biconditional lhs rhs -> "(" <> pretty lhs <> ")↔(" <> pretty rhs <> ")"
  Forall name body -> "∀" <> getName name <> "(" <> pretty body <> ")"
  Exists name body -> "∃" <> getName name <> "(" <> pretty body <> ")"
  Equality lhs rhs -> getName lhs <> "=" <> getName rhs

-- Substitute free instances of a given object name with another object name
substitute :: Name_Obj -> Name_Obj -> Formula -> Formula
substitute from to = impl
  where

  mapName_obj :: Name_Obj -> Name_Obj
  mapName_obj name = if name == from then to else name

  mapName_fitch :: Name_Fitch -> Name_Fitch
  mapName_fitch = case to of
    Name_Obj_FOL _ -> identity
    Name_Obj_Fitch to' -> \var -> if from == Name_Obj_Fitch var then to' else var

  impl = case _ of
    Empty -> Empty
    Bottom -> Bottom
    Declaration name -> Declaration (mapName_fitch name)
    Application name args -> Application name (map mapName_obj args)
    Negation body -> Negation (impl body)
    Conjunction lhs rhs -> Conjunction (impl lhs) (impl rhs)
    Disjunction lhs rhs -> Disjunction (impl lhs) (impl rhs)
    Implication lhs rhs -> Implication (impl lhs) (impl rhs)
    Biconditional lhs rhs -> Biconditional (impl lhs) (impl rhs)
    -- ↓ Recur on Forall/Exists unless variable name shadowed
    formula@(Forall arg body) -> if from == Name_Obj_FOL arg then formula else Forall arg (impl body)
    formula@(Exists arg body) -> if from == Name_Obj_FOL arg then formula else Exists arg (impl body)
    Equality lhs rhs -> Equality (mapName_obj lhs) (mapName_obj rhs)

-- ↓ Return all free (object-level) variables
freeVars :: Formula -> Set Name_Fitch
freeVars formula = case formula of
  Empty -> Set.empty
  Bottom -> Set.empty
  Declaration _ -> Set.empty
  Application _ args -> getFitch args
  Negation body -> freeVars body
  Conjunction lhs rhs -> freeVars lhs <> freeVars rhs
  Disjunction lhs rhs -> freeVars lhs <> freeVars rhs
  Implication lhs rhs -> freeVars lhs <> freeVars rhs
  Biconditional lhs rhs -> freeVars lhs <> freeVars rhs
  Forall _ body -> freeVars body
  Exists _ body -> freeVars body
  Equality lhs rhs -> getFitch [lhs, rhs]

  where

  getFitch :: Array Name_Obj -> Set Name_Fitch
  getFitch = Set.fromFoldable
         <<< Array.mapMaybe case _ of
                Name_Obj_Fitch var -> Just var
                Name_Obj_FOL _ -> Nothing

children :: Formula -> Array Formula
children formula = case formula of
  Empty -> []
  Bottom -> []
  Declaration _ -> []
  Application _ _ -> []
  Negation body -> [body]
  Conjunction lhs rhs -> [lhs, rhs]
  Disjunction lhs rhs -> [lhs, rhs]
  Implication lhs rhs -> [lhs, rhs]
  Biconditional lhs rhs -> [lhs, rhs]
  Forall _ body -> [body]
  Exists _ body -> [body]
  Equality _ _ -> []

