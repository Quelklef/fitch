module Formula exposing (..)

import Result

import StringUtil
import MaybeUtil
import ListUtil

import Parse exposing (..)

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

uniBottom = "⊥"
uniNot = "¬"
uniAnd = "∧"
uniOr = "∨"
uniIf = "→"
uniIff = "↔"
uniForall = "∀"
uniExists = "∃"
uniEqual = "="
uniInequal = "≠"

-- vv Normalize some code to a single syntax
normalize : Bool -> String -> String
normalize useUnicode code =
  tokenize code
  |> List.map (\token -> case token of
    TokInvalid text -> text
    TokIgnored text -> text
    TokOpen -> "("
    TokClose -> ")"
    TokBottom -> if useUnicode then uniBottom else "#"
    TokName name -> String.fromChar name
    TokDeclare name -> "[" ++ (String.fromChar name) ++ "]"
    TokNot -> if useUnicode then uniNot else "~"
    TokAnd -> if useUnicode then uniAnd else "&"
    TokOr -> if useUnicode then uniOr else "|"
    TokIf -> if useUnicode then uniIf else "->"
    TokIff -> if useUnicode then uniIff else "<->"
    TokForall -> if useUnicode then uniForall else "V"
    TokExists -> if useUnicode then uniExists else "E"
    TokEqual -> "="
    TokInequal -> if useUnicode then uniInequal else "/="
  )
  |> String.join ""

-- vv Tokens that map 1:1 to symbols
symbolMapping =
  [ ( "(", TokOpen )
  , ( ")", TokClose )

  , ( uniIf, TokIf )
  , ( "->", TokIf )

  , ( uniIff, TokIff )
  , ( "<->", TokIff )

  , ( uniBottom, TokBottom )
  , ( "_", TokBottom )
  , ( "#", TokBottom )

  , ( uniNot, TokNot )
  , ( "-", TokNot )
  , ( "~", TokNot )

  , ( uniAnd, TokAnd )
  , ( "&", TokAnd )
  , ( ".", TokAnd )
  , ( ".", TokAnd )

  , ( uniOr, TokOr )
  , ( "|", TokOr )
  , ( "v", TokOr )
  , ( "+", TokOr )

  , ( uniForall, TokForall )
  , ( "V", TokForall )

  , ( uniExists, TokExists )
  , ( "E", TokExists )

  , ( uniEqual, TokEqual )

  , ( uniInequal, TokInequal )
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
  |> MaybeUtil.orElseLazy (\() -> tokenizeWhitespace code)
  |> Maybe.withDefault (TokInvalid <| StringUtil.take 1 code, StringUtil.drop 1 code)

tokenize : String -> List Token
tokenize code =
  if String.length code == 0
  then []
  else let (token, rest) = tokenizeOne code
       in token :: tokenize rest

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
  with parseNameRaw <| \predName ->
  with (zeroPlus parseNameRaw) <| \varNames ->
  return <| Application predName varNames

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
parseForall = parseBinOp TokForall parseNameRaw parseBiconditional Forall
parseExists = parseBinOp TokExists parseNameRaw parseForall Exists

parseEmpty = eof |> kThen (return Empty)

parseTop =
  parseEmpty
  |> or parseBiconditional
  |> or parseExists
  |> or parseForall

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
