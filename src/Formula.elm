module Formula exposing (..)

import Set exposing (Set)

import StringUtil
import MaybeUtil
import ListUtil

import Types exposing (Formula(..))
import Parse exposing (..)
import TextStyle

type Token
  = TokInvalid String  -- invalid syntax
  | TokIgnored String  -- valid but ignored syntax
  | TokOpen            -- open parens
  | TokClose           -- close parens
  | TokBottom          -- bottom
  | TokName Char       -- variable name
  | TokDeclare Char    -- declaring a new variable
  | TokNot             -- negation
  | TokAnd             -- conjunction
  | TokOr              -- disjunction
  | TokIf              -- implication
  | TokIff             -- biconditional
  | TokForall          -- forall
  | TokExists          -- exists
  | TokEqual           -- equality
  | TokInequal         -- inequality

renderTokens : List Token -> String
renderTokens = String.join "" << List.map (\token -> case token of
    TokInvalid text -> text
    TokIgnored text -> text
    TokOpen         -> "("
    TokClose        -> ")"
    TokBottom       -> "⊥"
    TokName name    -> String.fromChar name
    TokDeclare name -> "[" ++ String.fromChar name ++ "]"
    TokNot          -> "¬"
    TokAnd          -> "∧"
    TokOr           -> "∨"
    TokIf           -> "→"
    TokIff          -> "↔"
    TokForall       -> "∀"
    TokExists       -> "∃"
    TokEqual        -> "="
    TokInequal      -> "≠")

-- vv Tokens that map 1:1 to symbols
-- vv (in order of precedence)
symbolMapping =
  [ ( "(", TokOpen )
  , ( ")", TokClose )

  , ( "→", TokIf )
  , ( "->", TokIf )
  -- vv These two strange rule is required because, as the user types, we replace
  -- vv what they're typing with a normalized unicode/ascii version. For instance, if
  -- vv the user has typed "P-", intending to type "P->Q", it will be eagerly
  -- vv replaced with "P¬" (if unicode is enabled) or "P~" (if it is not), since the
  -- vv program tokenizes it as P NOT.
  -- vv To Account for this, we need simply to include the following two rule.
  , ( "¬>", TokIf )
  , ( "~>", TokIf )

  , ( "↔", TokIff )
  -- vv Similar deal here as with the "¬>" rule
  , ( "<¬>", TokIff )
  , ( "<~>", TokIff )
  , ( "<->", TokIff )

  , ( "⊥", TokBottom )
  , ( "_", TokBottom )
  , ( "#", TokBottom )

  , ( "¬", TokNot )
  , ( "-", TokNot )
  , ( "~", TokNot )
  , ( "!", TokNot )

  , ( "∧", TokAnd )
  , ( "&", TokAnd )
  , ( ".", TokAnd )
  , ( ".", TokAnd )

  , ( "∨", TokOr )
  , ( "|", TokOr )
  , ( "v", TokOr )
  , ( "+", TokOr )

  , ( "∀", TokForall )
  , ( "\\", TokForall )
  , ( "V", TokForall )

  , ( "∃", TokExists )
  , ( "@", TokExists )
  , ( "E", TokExists )

  , ( "=", TokEqual )

  , ( "≠", TokInequal )
  , ( "!=", TokInequal )
  , ( "/=", TokInequal )
  ]

chompChar : String -> Maybe (Char, String)
chompChar = String.uncons

tokenizeName : String -> Maybe (Token, String)
tokenizeName code =
  chompChar code
  |> Maybe.andThen (\(name, rest) ->
    if Char.isAlpha name then Just (TokName name, rest) else Nothing)

tokenizeDeclaration : String -> Maybe (Token, String)
tokenizeDeclaration code =
  let prefix = StringUtil.get 0 code
      char = StringUtil.get 1 code
      suffix = StringUtil.get 2 code
      rest = StringUtil.drop 3 code
  in case (prefix, char, suffix) of
    (Just '[', Just name, Just ']') -> Just (TokDeclare name, rest)
    _ -> Nothing

tokenizeSymbol : String -> Maybe (Token, String)
tokenizeSymbol code =
  symbolMapping
  |> List.filterMap (\(string, token) ->
    if String.startsWith string code
    then Just (token, StringUtil.drop (String.length string) code)
    else Nothing
  )
  |> List.head

tokenizeWhitespace : String -> Maybe (Token, String)
tokenizeWhitespace code =
  let whitespace = code |> StringUtil.takeWhile (\char -> char == ' ')
      rest = StringUtil.drop (String.length whitespace) code
  in if whitespace /= ""
     then Just (TokIgnored whitespace, rest)
     else Nothing

tokenizeOne : String -> (Token, String)
tokenizeOne code =
  tokenizeSymbol code
  |> MaybeUtil.orElseLazy (\() -> tokenizeName code)
  |> MaybeUtil.orElseLazy (\() -> tokenizeDeclaration code)
  |> MaybeUtil.orElseLazy (\() -> tokenizeWhitespace code)
  |> Maybe.withDefault (TokInvalid <| StringUtil.take 1 code, StringUtil.drop 1 code)

tokenize : String -> List Token
tokenize code =
  if String.length code == 0
  then []
  else let (token, rest) = tokenizeOne code
       in token :: tokenize rest

-- --

parseBottom : Parser Token Formula
parseBottom = literal [TokBottom] |> kThen (return Bottom)

parseDeclaration : Parser Token Formula
parseDeclaration = withM takeOne (\token -> case token of
  TokDeclare name -> Just (return <| Declaration name)
  _ -> Nothing)

parseNameRaw : Parser Token Char
parseNameRaw = withM takeOne (\token -> case token of
  TokName name -> Just (return name)
  _ -> Nothing)

parseName : Parser Token Formula
parseName = parseNameRaw >> Maybe.map (\(name, rest) -> (Name name, rest))

parseApplication : Parser Token Formula
parseApplication =
  with parseNameRaw <| \head ->
  with (zeroPlus parseNameRaw) <| \tail ->
    return <| case head :: tail of
      -- vv As a special rule, allow aRb to mean Rab
      -- vv Works only on exactly 3 names in a row following pattern lowercase-uppercase-lowercase
      [a, r, b] ->
        if Char.isLower a && Char.isUpper r && Char.isLower b
        then Application r [a, b]
        else Application a [r, b]
      _ -> Application head tail

parseNegation : Parser Token Formula
parseNegation = literal [TokNot] |> kThen (with (lazy (\() -> parseTop)) (return << Negation))

parseParenthesized : Parser Token Formula
parseParenthesized =
  literal [TokOpen]
  |> kThen (with (lazy (\() -> parseTop)) <| \body ->
  literal [TokClose]
  |> kThen (return body))

parseBinOp : tok -> Parser tok lhs -> Parser tok rhs -> (lhs -> rhs -> res) -> Parser tok res
parseBinOp opToken lhsParser rhsParser makeResult =
  with lhsParser <| \lhs ->
  literal [opToken]
  |> kThen (with rhsParser <| \rhs ->
  return (makeResult lhs rhs))

parseEquality = parseBinOp TokEqual parseNameRaw parseNameRaw Equality
parseInequality = parseBinOp TokInequal parseNameRaw parseNameRaw (\lhs rhs -> Negation (Equality lhs rhs))

parseSimple : Parser Token Formula
parseSimple =
  parseBottom
  |> or parseNegation
  |> or parseEquality
  |> or parseInequality
  |> or parseParenthesized
  |> or parseApplication
  |> or parseDeclaration

parseBinOpWithFallthrough : tok -> Parser tok res -> (res -> res -> res) -> Parser tok res
parseBinOpWithFallthrough opToken innerParser makeResult =
  with innerParser (\lhs ->
  with peekOne (\operator ->
  if operator /= Just opToken
  then return lhs
  else takeOne
       |> kThen (with innerParser <| \rhs ->
       return (makeResult lhs rhs))))

parseConjunction = parseBinOpWithFallthrough TokAnd parseSimple Conjunction
parseDisjunction = parseBinOpWithFallthrough TokOr parseConjunction Disjunction
parseImplication = parseBinOpWithFallthrough TokIf parseDisjunction Implication
parseBiconditional = parseBinOpWithFallthrough TokIff parseImplication Biconditional

parseForall =
  literal [TokForall]
  |> kThen (with parseNameRaw <| \name ->
  with parseTop <| \body ->
  return (Forall name body))

parseExists =
  literal [TokExists]
  |> kThen (with parseNameRaw <| \name ->
  with parseTop <| \body ->
  return (Exists name body))

parseEmpty = eof |> kThen (return Empty)

parseTop =
  parseEmpty
  |> or parseBiconditional
  |> or parseForall
  |> or parseExists

parseTokens : List Token -> Maybe Formula
parseTokens tokens =
  let isMeaningful token = case token of
        TokInvalid _ -> False
        TokIgnored _ -> False
        _ -> True
      isValid token = case token of
        TokInvalid _ -> False
        _ -> True
      meaningfulTokens () = List.filter isMeaningful tokens
      hasInvalid = List.any (not << isValid) tokens
  in if hasInvalid
     then Nothing
     else meaningfulTokens ()
          |> with parseTop (\result -> eof |> kThen (return result))
          |> Maybe.map (\(result, rest) -> result)

parse : String -> Maybe Formula
parse = tokenize >> parseTokens

-- --

prettify : Bool -> String -> String
prettify useUnicode = tokenize >> renderTokens >> TextStyle.map useUnicode

substitute : Char -> Char -> Formula -> Formula
substitute from to formula =
  let mapName str = if str == from then to else str
  in case formula of
    Empty -> Empty
    Bottom -> Bottom
    Declaration name -> Declaration (mapName name)
    Application name args -> Application name (List.map mapName args)
    Name name -> Name (mapName name)
    Negation body -> Negation (substitute from to body)
    Conjunction lhs rhs -> Conjunction (substitute from to lhs) (substitute from to rhs)
    Disjunction lhs rhs -> Disjunction (substitute from to lhs) (substitute from to rhs)
    Implication lhs rhs -> Implication (substitute from to lhs) (substitute from to rhs)
    Biconditional lhs rhs -> Biconditional (substitute from to lhs) (substitute from to rhs)
    -- vv Recur on Forall/Exists unless variable name shadowed
    Forall arg body -> if arg == from then formula else Forall arg (substitute from to body)
    Exists arg body -> if arg == from then formula else Exists arg (substitute from to body)
    Equality lhs rhs -> Equality (mapName lhs) (mapName rhs)

-- vv Return all free variables which do not represent predicates or propositions
freeObjectVars : Formula -> Set Char
freeObjectVars formula = case formula of
  Empty -> Set.empty
  Bottom -> Set.empty
  -- vv Declared variables are not considered to be free
  Declaration name -> Set.empty
  -- vv Predicate variables are not included
  Application name args -> Set.fromList args
  Name name -> Set.singleton name
  Negation body -> freeObjectVars body
  Conjunction lhs rhs -> Set.union (freeObjectVars lhs) (freeObjectVars rhs)
  Disjunction lhs rhs -> Set.union (freeObjectVars lhs) (freeObjectVars rhs)
  Implication lhs rhs -> Set.union (freeObjectVars lhs) (freeObjectVars rhs)
  Biconditional lhs rhs -> Set.union (freeObjectVars lhs) (freeObjectVars rhs)
  Forall arg body -> Set.remove arg (freeObjectVars body)
  Exists arg body -> Set.remove arg (freeObjectVars body)
  Equality lhs rhs -> Set.fromList [lhs, rhs]
