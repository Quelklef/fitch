module Fitch.Formula where

import Prelude
import Data.Array as Array
import Data.Set as Set
import Data.Set (Set)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (intercalate)
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as String
import Data.String.Utils (startsWith) as String
import Data.CodePoint.Unicode as CodePoint

import Fitch.Types (Formula (..))
import Fitch.TextStyle as TextStyle
import Fitch.Util.Parse
import Fitch.Util.StringUtil as StringUtil
import Fitch.Util.MaybeUtil as MaybeUtil

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
      rest = StringUtil.drop 3 code
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
    then Just $ [token] /\ StringUtil.drop (String.length string) code
    else Nothing
  )
  # Array.head

tokenizeWhitespace :: String -> Maybe (Array Token /\ String)
tokenizeWhitespace code =
  let whitespace = code # StringUtil.takeWhile (\char -> char == String.codePointFromChar ' ')
      rest = StringUtil.drop (String.length whitespace) code
  in if whitespace /= ""
     then Just $ [TokIgnored whitespace] /\ rest
     else Nothing

tokenizeOne :: String -> (Array Token /\ String)
tokenizeOne code =
  tokenizeSymbol code
  # MaybeUtil.orElseLazy (\_ -> tokenizeName code)
  # MaybeUtil.orElseLazy (\_ -> tokenizeDeclaration code)
  # MaybeUtil.orElseLazy (\_ -> tokenizeWhitespace code)
  # fromMaybe ([TokInvalid $ StringUtil.take 1 code] /\ StringUtil.drop 1 code)

tokenize :: String -> Array Token
tokenize code =
  if String.length code == 0
  then []
  else let tokens /\ rest = tokenizeOne code
       in tokens <> tokenize rest

-- --

parseBottom :: Parser Token Formula
parseBottom = literal [TokBottom] # kThen (return Bottom)

parseDeclaration :: Parser Token Formula
parseDeclaration = withM takeOne (\token -> case token of
  TokDeclare name -> Just (return $ Declaration name)
  _ -> Nothing)

parseNameRaw :: Parser Token CodePoint
parseNameRaw = withM takeOne (\token -> case token of
  TokName name -> Just (return name)
  _ -> Nothing)

parseName :: Parser Token Formula
parseName = parseNameRaw >>> map (\(name /\ rest) -> Name name /\ rest)

parseApplication :: Parser Token Formula
parseApplication =
  with parseNameRaw $ \head ->
  with (zeroPlus parseNameRaw) $ \tail ->
    return $ case Array.cons head tail of
      -- ↓ As a special rule, allow aRb to mean Rab
      -- ↓ Works only on exactly 3 names in a row following pattern lowercase-uppercase-lowercase
      [a, r, b] ->
        if CodePoint.isLower a && CodePoint.isUpper r && CodePoint.isLower b
        then Application r [a, b]
        else Application a [r, b]
      _ -> Application head tail

parseNegation :: Parser Token Formula
parseNegation = literal [TokNot] # kThen (with (lazy $ \_ -> parseNonBinOp) (return <<< Negation))

parseParenthesized :: Parser Token Formula
parseParenthesized =
  literal [TokOpen]
  # kThen (with (lazy $ \_ -> parseTop) $ \body ->
  literal [TokClose]
  # kThen (return body))

parseBinOp :: forall tok lhs rhs res. Eq tok => tok -> Parser tok lhs -> Parser tok rhs -> (lhs -> rhs -> res) -> Parser tok res
parseBinOp opToken lhsParser rhsParser makeResult =
  with lhsParser $ \lhs ->
  literal [opToken]
  # kThen (with rhsParser $ \rhs ->
  return (makeResult lhs rhs))

parseEquality = parseBinOp TokEqual parseNameRaw parseNameRaw Equality
parseInequality = parseBinOp TokInequal parseNameRaw parseNameRaw (\lhs rhs -> Negation (Equality lhs rhs))

parseForall =
  literal [TokForall]
  # kThen (with parseNameRaw $ \name ->
  with parseNonBinOp $ \body ->
  return (Forall name body))

parseExists =
  literal [TokExists]
  # kThen (with parseNameRaw $ \name ->
  with parseNonBinOp $ \body ->
  return (Exists name body))

parseEmpty = eof # kThen (return Empty)

parseNonBinOp :: Parser Token Formula
parseNonBinOp =
  (\t -> parseEmpty t)
  # or (\t -> parseBottom t)
  # or (\t -> parseNegation t)
  # or (\t -> parseEquality t)
  # or (\t -> parseInequality t)
  # or (\t -> parseParenthesized t)
  # or (\t -> parseApplication t)
  # or (\t -> parseDeclaration t)
  # or (\t -> parseForall t)
  # or (\t -> parseExists t)

parseBinOpWithFallthrough :: forall tok res. Eq tok => tok -> Parser tok res -> (res -> res -> res) -> Parser tok res
parseBinOpWithFallthrough opToken innerParser makeResult =
  with innerParser (\lhs ->
  with (peek 1) (\operator ->
  if operator /= [opToken]
  then return lhs
  else takeOne
       # kThen (with innerParser $ \rhs ->
       return (makeResult lhs rhs))))

parseConjunction = (\t -> parseBinOpWithFallthrough t) TokAnd (\t -> parseNonBinOp t) Conjunction
parseDisjunction = (\t -> parseBinOpWithFallthrough t) TokOr (\t -> parseConjunction t) Disjunction
parseImplication = (\t -> parseBinOpWithFallthrough t) TokIf (\t -> parseDisjunction t) Implication
parseBiconditional = (\t -> parseBinOpWithFallthrough t) TokIff (\t -> parseImplication t) Biconditional

parseTop = (\t -> parseBiconditional t)

parseTokens :: Array Token -> Maybe Formula
parseTokens tokens =
  let isMeaningful token = case token of
        TokInvalid _ -> false
        TokIgnored _ -> false
        _ -> true
      isValid token = case token of
        TokInvalid _ -> false
        _ -> true
      meaningfulTokens _ = Array.filter isMeaningful tokens
      hasInvalid = Array.any (not <<< isValid) tokens
  in if hasInvalid
     then Nothing
     else meaningfulTokens unit
          # with parseTop (\result -> eof # kThen (return result))
          # map (\(result /\ rest) -> result)

parse :: String -> Maybe Formula
parse = tokenize >>> parseTokens

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

