module Fitch.Formula where

import Prelude
import Data.Array as Array
import Data.Either (Either (..), hush)
import Data.Set as Set
import Data.Set (Set)
import Data.Maybe (Maybe (..))
import Data.Tuple.Nested ((/\))
import Data.Foldable (intercalate)
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints (drop, length, singleton, take, stripPrefix, codePointAt) as String
import Data.String.Pattern (Pattern (..)) as String
import Data.String.Utils (filter) as String
import Data.String.CodeUnits as StringU
import Data.CodePoint.Unicode (isAlpha, isLower, isUpper)
import Control.Lazy (defer)
import Text.Parsing.StringParser (Parser (..), fail, runParser, try)
import Text.Parsing.StringParser.CodePoints (eof, string)
import Text.Parsing.StringParser.CodeUnits (anyChar)
import Text.Parsing.StringParser.Combinators (choice, many)

import Fitch.Types (Formula (..))


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


parse :: String -> Maybe Formula
parse =

    \str -> hush $ runParser (parseImpl <* eof) (String.filter (_ /= " ") str)

  where

  anyCodePoint :: Parser CodePoint
  anyCodePoint = Parser \{ str, pos } ->
    case String.codePointAt 0 (StringU.drop pos str) of
      Just cp -> Right { result: cp, suffix: { str, pos: pos + String.length (String.singleton cp) } }
      Nothing -> Left { pos, error: "Unexpected EOF" }

  parseBottom :: Parser Formula
  parseBottom = string "⊥" $> Bottom

  parseDeclaration :: Parser Formula
  parseDeclaration = do
    void $ string "["
    char <- anyCodePoint
    void $ string "]"

    -- ↓ As a special case, allow comments after declarations
    void $ many anyChar

    if isAlpha char
    then pure (Declaration char)
    else fail "Expected declaration"

  parseNameRaw :: Parser CodePoint
  parseNameRaw = do
    char <- anyCodePoint
    if isAlpha char
    then pure char
    else fail "Expected name"

  parseApplication :: Parser Formula
  parseApplication = do
    head <- parseNameRaw
    tail <- Array.many (try parseNameRaw)
    pure $ case Array.cons head tail of
      -- ↓ As a special rule, allow aRb to mean Rab
      --   Works only on exactly 3 names in a row following pattern lowercase-uppercase-lowercase
      [a, r, b] ->
        if isLower a && isUpper r && isLower b
        then Application r [a, b]
        else Application a [r, b]
      _ -> Application head tail

  parseNegation = defer \_ -> string "¬" *> (Negation <$> parseNonBinOp)

  parseParenthesized = defer \_ -> string "(" *> parseImpl <* string ")"

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

  parseEquality = parseBinOp "=" parseNameRaw parseNameRaw Equality
  parseInequality = parseBinOp "≠" parseNameRaw parseNameRaw (\lhs rhs -> Negation (Equality lhs rhs))

  parseForall = do
    void $ string "∀"
    name <- parseNameRaw
    body <- parseNonBinOp
    pure $ Forall name body

  parseExists = do
    void $ string "∃"
    name <- parseNameRaw
    body <- parseNonBinOp
    pure $ Exists name body

  parseEmpty :: Parser Formula
  parseEmpty = eof $> Empty

  parseNonBinOp :: Parser Formula
  parseNonBinOp = defer \_ ->
    choice <<< map try $
      [ parseEmpty
      , parseBottom
      , parseNegation
      , parseEquality
      , parseInequality
      , parseParenthesized
      , parseApplication
      , parseDeclaration
      , parseForall
      , parseExists
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

  parseConjunction = defer \_ -> parseBinOpWithFallthrough "∧" parseNonBinOp Conjunction
  parseDisjunction = defer \_ -> parseBinOpWithFallthrough "∨" parseConjunction Disjunction
  parseImplication = defer \_ -> parseBinOpWithFallthrough "→" parseDisjunction Implication
  parseBiconditional = defer \_ -> parseBinOpWithFallthrough "↔" parseImplication Biconditional

  parseImpl :: Parser Formula
  parseImpl = defer \_ -> parseBiconditional


pretty :: Formula -> String
pretty formula = case formula of
  Empty -> ""
  Bottom -> "⊥"
  Declaration name -> "[" <> String.singleton name <> "]"
  Application name args -> String.singleton name <> intercalate "" (map String.singleton args)
  Negation body -> "¬(" <> pretty body <> ")"
  Conjunction lhs rhs -> "(" <> pretty lhs <> ")∧(" <> pretty rhs <> ")"
  Disjunction lhs rhs -> "(" <> pretty lhs <> ")∨(" <> pretty rhs <> ")"
  Implication lhs rhs -> "(" <> pretty lhs <> ")→(" <> pretty rhs <> ")"
  Biconditional lhs rhs -> "(" <> pretty lhs <> ")↔(" <> pretty rhs <> ")"
  Forall name body -> "∀" <> String.singleton name <> "(" <> pretty body <> ")"
  Exists name body -> "∃" <> String.singleton name <> "(" <> pretty body <> ")"
  Equality lhs rhs -> String.singleton lhs <> "=" <> String.singleton rhs

substitute :: CodePoint -> CodePoint -> Formula -> Formula
substitute from to formula =
  case formula of
    Empty -> Empty
    Bottom -> Bottom
    Declaration name -> Declaration (mapName name)
    Application name args -> Application name (map mapName args)
    Negation body -> Negation (substitute from to body)
    Conjunction lhs rhs -> Conjunction (substitute from to lhs) (substitute from to rhs)
    Disjunction lhs rhs -> Disjunction (substitute from to lhs) (substitute from to rhs)
    Implication lhs rhs -> Implication (substitute from to lhs) (substitute from to rhs)
    Biconditional lhs rhs -> Biconditional (substitute from to lhs) (substitute from to rhs)
    -- ↓ Recur on Forall/Exists unless variable name shadowed
    Forall arg body -> if arg == from then formula else Forall arg (substitute from to body)
    Exists arg body -> if arg == from then formula else Exists arg (substitute from to body)
    Equality lhs rhs -> Equality (mapName lhs) (mapName rhs)

  where mapName str = if str == from then to else str

-- ↓ Return all free variables which do not represent predicates or propositions
freeObjectVars :: Formula -> Set CodePoint
freeObjectVars formula = case formula of
  Empty -> Set.empty
  Bottom -> Set.empty
  -- ↓ Declared variables are not considered to be free
  Declaration _name -> Set.empty
  -- ↓ Predicate variables are not included
  Application _name args -> Set.fromFoldable args
  Negation body -> freeObjectVars body
  Conjunction lhs rhs -> Set.union (freeObjectVars lhs) (freeObjectVars rhs)
  Disjunction lhs rhs -> Set.union (freeObjectVars lhs) (freeObjectVars rhs)
  Implication lhs rhs -> Set.union (freeObjectVars lhs) (freeObjectVars rhs)
  Biconditional lhs rhs -> Set.union (freeObjectVars lhs) (freeObjectVars rhs)
  Forall arg body -> Set.delete arg (freeObjectVars body)
  Exists arg body -> Set.delete arg (freeObjectVars body)
  Equality lhs rhs -> Set.fromFoldable [lhs, rhs]

declaring :: Formula -> Set CodePoint
declaring formula = case formula of
  Declaration name -> Set.singleton name
  Forall name _ -> Set.singleton name
  Exists name _ -> Set.singleton name
  _ -> Set.empty

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

