module Fitch.Formula where

import Prelude
import Data.Array as Array
import Data.List as List
import Data.List (List)
import Data.Either (Either (..), hush)
import Data.Set as Set
import Data.Set (Set)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (intercalate, any)
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as String
import Data.String.Utils (startsWith) as String
import Data.CodePoint.Unicode as CodePoint
import Data.Generic.Rep (class Generic)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Control.Alt ((<|>))
import Control.Monad.State.Trans (get, put)
import Text.Parsing.Parser (Parser, ParseState (..), ParseError (..), fail, runParser, position, failWithPosition)
import Text.Parsing.Parser.Pos (Position, initialPos)
import Text.Parsing.Parser.Token (match, token) as Parser
import Text.Parsing.Parser.Combinators (choice, try)

import Fitch.Types (Formula (..))
import Fitch.TextStyle as TextStyle
import Fitch.Util.StringUtil as StringUtil
import Fitch.Util.MaybeUtil as MaybeUtil
import Fitch.Util.Parsing (match, token, eof, unlazy)

data Token
  = TokInvalid String    -- invalid syntax
  | TokIgnored String    -- valid but meaningless syntax, such as whitespace
  | TokOpen              -- open parens
  | TokClose             -- close parens
  | TokBottom            -- bottom
  | TokName CodePoint    -- variable name
  | TokDeclare CodePoint -- declaring a new variable
  | TokNot               -- negation
  | TokAnd               -- conjunction
  | TokOr                -- disjunction
  | TokIf                -- implication
  | TokIff               -- biconditional
  | TokForall            -- forall
  | TokExists            -- exists
  | TokEqual             -- equality
  | TokInequal           -- inequality

derive instance Eq Token
derive instance Generic Token _
instance Show Token where show = genericShow

renderTokens :: Array Token -> String
renderTokens = intercalate "" <<< map (\token -> case token of
    TokInvalid text -> text
    TokIgnored text -> text
    TokOpen         -> "("
    TokClose        -> ")"
    TokBottom       -> "⊥"
    TokName name    -> String.singleton name
    TokDeclare name -> "[" <> String.singleton name <> "]"
    TokNot          -> "¬"
    TokAnd          -> "∧"
    TokOr           -> "∨"
    TokIf           -> "→"
    TokIff          -> "↔"
    TokForall       -> "∀"
    TokExists       -> "∃"
    TokEqual        -> "="
    TokInequal      -> "≠")

-- ↓ Tokens that map 1:1 to symbols
-- ↓ (in order of precedence)
symbolMapping =
  [ ( "(" /\ TokOpen )
  , ( ")" /\ TokClose )

  , ( "→" /\ TokIf )
  , ( ">" /\ TokIf )

  , ( "↔" /\ TokIff )
  , ( "<>" /\ TokIff )

  , ( "⊥" /\ TokBottom )
  , ( "_" /\ TokBottom )
  , ( "#" /\ TokBottom )

  , ( "¬" /\ TokNot )
  , ( "-" /\ TokNot )
  , ( "~" /\ TokNot )
  , ( "!" /\ TokNot )

  , ( "∧" /\ TokAnd )
  , ( "&" /\ TokAnd )
  , ( "*" /\ TokAnd )
  , ( "." /\ TokAnd )

  , ( "∨" /\ TokOr )
  , ( "|" /\ TokOr )
  , ( "v" /\ TokOr )
  , ( "+" /\ TokOr )

  , ( "∀" /\ TokForall )
  , ( "\\" /\ TokForall )
  , ( "V" /\ TokForall )

  , ( "∃" /\ TokExists )
  , ( "@" /\ TokExists )
  , ( "E" /\ TokExists )

  , ( "=" /\ TokEqual )

  , ( "≠" /\ TokInequal )
  , ( "!=" /\ TokInequal )
  , ( "/=" /\ TokInequal )
  ]

chompCodePoint :: String -> Maybe (CodePoint /\ String)
chompCodePoint str = String.uncons str # map (\{ head, tail } -> head /\ tail)

tokenizeName :: String -> Maybe (Array Token /\ String)
tokenizeName code =
  chompCodePoint code
  >>= (\(name /\ rest) ->
    if CodePoint.isAlpha name then Just ([TokName name] /\ rest) else Nothing)

-- ↓ Tokenize a declaration.
-- ↓ Note that, as a special case, all code following a valid declaration
-- ↓ will be turned into an 'ignored' token.
-- ↓ This is to allow users to write comments on their declarations!
tokenizeDeclaration :: String -> Maybe (Array Token /\ String)
tokenizeDeclaration code =
  let prefix = StringUtil.get 0 code
      char = StringUtil.get 1 code
      suffix = StringUtil.get 2 code
      rest = String.drop 3 code
  in
    case char of
      Just name ->
        if prefix == Just (String.codePointFromChar '[')
        && suffix == Just (String.codePointFromChar ']')
        then Just $ [TokDeclare name, TokIgnored rest] /\ ""
        else Nothing
      Nothing -> Nothing

tokenizeSymbol :: String -> Maybe (Array Token /\ String)
tokenizeSymbol code =
  symbolMapping
  # Array.mapMaybe (\(string /\ token) ->
    if String.startsWith string code
    then Just $ [token] /\ String.drop (String.length string) code
    else Nothing
  )
  # Array.head

tokenizeWhitespace :: String -> Maybe (Array Token /\ String)
tokenizeWhitespace code =
  let whitespace = code # String.takeWhile (\char -> char == String.codePointFromChar ' ')
      rest = String.drop (String.length whitespace) code
  in if whitespace /= ""
     then Just $ [TokIgnored whitespace] /\ rest
     else Nothing

tokenizeOne :: String -> (Array Token /\ String)
tokenizeOne code =
  tokenizeSymbol code
  # MaybeUtil.orElseLazy (\_ -> tokenizeName code)
  # MaybeUtil.orElseLazy (\_ -> tokenizeDeclaration code)
  # MaybeUtil.orElseLazy (\_ -> tokenizeWhitespace code)
  # fromMaybe ([TokInvalid $ String.take 1 code] /\ String.drop 1 code)

tokenize :: String -> Array Token
tokenize code =
  if String.length code == 0
  then []
  else let tokens /\ rest = tokenizeOne code
       in tokens <> tokenize rest

-- --

parseBottom :: Parser (List Token) Formula
parseBottom = match TokBottom $> Bottom

parseDeclaration :: Parser (List Token) Formula
parseDeclaration = do
  token >>= case _ of
    TokDeclare name -> pure (Declaration name)
    _ -> fail "Expected declaration"

parseNameRaw :: Parser (List Token) CodePoint
parseNameRaw = do
  token >>= case _ of
    TokName name -> pure name
    _ -> fail "Expected name"

parseName :: Parser (List Token) Formula
parseName = Name <$> parseNameRaw

parseApplication :: Parser (List Token) Formula
parseApplication = do
  head <- parseNameRaw
  tail <- Array.many (try parseNameRaw)
  pure $ case Array.cons head tail of
    -- ↓ As a special rule, allow aRb to mean Rab
    -- ↓ Works only on exactly 3 names in a row following pattern lowercase-uppercase-lowercase
    [a, r, b] ->
      if CodePoint.isLower a && CodePoint.isUpper r && CodePoint.isLower b
      then Application r [a, b]
      else Application a [r, b]
    _ -> Application head tail

parseNegation :: Parser (List Token) Formula
parseNegation = unlazy \_ ->
                match TokNot *> (Negation <$> parseNonBinOp)

parseParenthesized :: Parser (List Token) Formula
parseParenthesized = unlazy \_ ->
                     match TokOpen *> parseTop <* match TokClose

parseBinOp ::
  forall tok lhs rhs res
  .  Eq tok
  => tok
  -> Parser (List tok) lhs
  -> Parser (List tok) rhs
  -> (lhs -> rhs -> res)
  -> Parser (List tok) res
parseBinOp opToken lhsParser rhsParser makeResult = do
  lhs <- lhsParser
  void $ match opToken
  rhs <- rhsParser
  pure $ makeResult lhs rhs

parseEquality = parseBinOp TokEqual parseNameRaw parseNameRaw Equality
parseInequality = parseBinOp TokInequal parseNameRaw parseNameRaw (\lhs rhs -> Negation (Equality lhs rhs))

parseForall = do
  void $ match TokForall
  name <- parseNameRaw
  body <- parseNonBinOp
  pure $ Forall name body

parseExists = do
  void $ match TokExists
  name <- parseNameRaw
  body <- parseNonBinOp
  pure $ Exists name body

parseEmpty = eof $> Empty

parseNonBinOp :: Parser (List Token) Formula
parseNonBinOp = unlazy \_ ->
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
  forall tok res
  .  Show tok => Eq tok
  => tok
  -> Parser (List tok) res
  -> (res -> res -> res)
  -> Parser (List tok) res
parseBinOpWithFallthrough opToken innerParser makeResult = do
  val <- innerParser
  choice <<< map try $
    [ do let lhs = val
         void $ match opToken
         rhs <- innerParser
         pure $ makeResult lhs rhs
    , do pure val
    ]

parseConjunction = unlazy \_ -> parseBinOpWithFallthrough TokAnd parseNonBinOp Conjunction
parseDisjunction = unlazy \_ -> parseBinOpWithFallthrough TokOr parseConjunction Disjunction
parseImplication = unlazy \_ -> parseBinOpWithFallthrough TokIf parseDisjunction Implication
parseBiconditional = unlazy \_ -> parseBinOpWithFallthrough TokIff parseImplication Biconditional

parseTop :: Parser (List Token) Formula
parseTop = unlazy \_ -> parseBiconditional

parseTokens :: List Token -> Maybe Formula
parseTokens tokens =
  if any (not <<< isValid) tokens then Nothing
  else hush $ runParser (List.filter isMeaningful tokens) (parseTop <* eof)

  where

  isMeaningful = case _ of
    TokInvalid _ -> false
    TokIgnored _ -> false
    _ -> true

  isValid = case _ of
    TokInvalid _ -> false
    _ -> true

parse :: String -> Maybe Formula
parse = tokenize >>> List.fromFoldable >>> parseTokens

-- --

prettifyText :: Boolean -> String -> String
prettifyText useUnicode = tokenize >>> renderTokens >>> TextStyle.map useUnicode

pretty :: Formula -> String
pretty formula = case formula of
  Empty -> ""
  Bottom -> "⊥"
  Name name -> String.singleton name
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
  let mapName str = if str == from then to else str
  in case formula of
    Empty -> Empty
    Bottom -> Bottom
    Declaration name -> Declaration (mapName name)
    Application name args -> Application name (map mapName args)
    Name name -> Name (mapName name)
    Negation body -> Negation (substitute from to body)
    Conjunction lhs rhs -> Conjunction (substitute from to lhs) (substitute from to rhs)
    Disjunction lhs rhs -> Disjunction (substitute from to lhs) (substitute from to rhs)
    Implication lhs rhs -> Implication (substitute from to lhs) (substitute from to rhs)
    Biconditional lhs rhs -> Biconditional (substitute from to lhs) (substitute from to rhs)
    -- ↓ Recur on Forall/Exists unless variable name shadowed
    Forall arg body -> if arg == from then formula else Forall arg (substitute from to body)
    Exists arg body -> if arg == from then formula else Exists arg (substitute from to body)
    Equality lhs rhs -> Equality (mapName lhs) (mapName rhs)

-- ↓ Return all free variables which do not represent predicates or propositions
freeObjectVars :: Formula -> Set CodePoint
freeObjectVars formula = case formula of
  Empty -> Set.empty
  Bottom -> Set.empty
  -- ↓ Declared variables are not considered to be free
  Declaration name -> Set.empty
  -- ↓ Predicate variables are not included
  Application _name args -> Set.fromFoldable args
  Name name -> Set.singleton name
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
  Name _ -> []
  Negation body -> [body]
  Conjunction lhs rhs -> [lhs, rhs]
  Disjunction lhs rhs -> [lhs, rhs]
  Implication lhs rhs -> [lhs, rhs]
  Biconditional lhs rhs -> [lhs, rhs]
  Forall _ body -> [body]
  Exists _ body -> [body]
  Equality _ _ -> []

