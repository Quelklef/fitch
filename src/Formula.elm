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
  | TokIgnored String  -- valid but meaningless syntax, such as whitespace
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
  , ( ">", TokIf )

  , ( "↔", TokIff )
  , ( "<>", TokIff )

  , ( "⊥", TokBottom )
  , ( "_", TokBottom )
  , ( "#", TokBottom )

  , ( "¬", TokNot )
  , ( "-", TokNot )
  , ( "~", TokNot )
  , ( "!", TokNot )

  , ( "∧", TokAnd )
  , ( "&", TokAnd )
  , ( "*", TokAnd )
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

tokenizeName : String -> Maybe (List Token, String)
tokenizeName code =
  chompChar code
  |> Maybe.andThen (\(name, rest) ->
    if Char.isAlpha name then Just ([TokName name], rest) else Nothing)

-- vv Tokenize a declaration.
-- vv Note that, as a special case, all code following a valid declaration
-- vv will be turned into an 'ignored' token.
-- vv This is to allow users to write comments on their declarations!
tokenizeDeclaration : String -> Maybe (List Token, String)
tokenizeDeclaration code =
  let prefix = StringUtil.get 0 code
      char = StringUtil.get 1 code
      suffix = StringUtil.get 2 code
      rest = StringUtil.drop 3 code
  in case (prefix, char, suffix) of
    (Just '[', Just name, Just ']') -> Just ([TokDeclare name, TokIgnored rest], "")
    _ -> Nothing

tokenizeSymbol : String -> Maybe (List Token, String)
tokenizeSymbol code =
  symbolMapping
  |> List.filterMap (\(string, token) ->
    if String.startsWith string code
    then Just ([token], StringUtil.drop (String.length string) code)
    else Nothing
  )
  |> List.head

tokenizeWhitespace : String -> Maybe (List Token, String)
tokenizeWhitespace code =
  let whitespace = code |> StringUtil.takeWhile (\char -> char == ' ')
      rest = StringUtil.drop (String.length whitespace) code
  in if whitespace /= ""
     then Just ([TokIgnored whitespace], rest)
     else Nothing

tokenizeOne : String -> (List Token, String)
tokenizeOne code =
  tokenizeSymbol code
  |> MaybeUtil.orElseLazy (\() -> tokenizeName code)
  |> MaybeUtil.orElseLazy (\() -> tokenizeDeclaration code)
  |> MaybeUtil.orElseLazy (\() -> tokenizeWhitespace code)
  |> Maybe.withDefault ([TokInvalid <| StringUtil.take 1 code], StringUtil.drop 1 code)

tokenize : String -> List Token
tokenize code =
  if String.length code == 0
  then []
  else let (tokens, rest) = tokenizeOne code
       in tokens ++ tokenize rest

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
parseNegation = literal [TokNot] |> kThen (with (lazy (\() -> parseNonBinOp)) (return << Negation))

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

parseForall =
  literal [TokForall]
  |> kThen (with parseNameRaw <| \name ->
  with parseNonBinOp <| \body ->
  return (Forall name body))

parseExists =
  literal [TokExists]
  |> kThen (with parseNameRaw <| \name ->
  with parseNonBinOp <| \body ->
  return (Exists name body))

parseEmpty = eof |> kThen (return Empty)

parseNonBinOp : Parser Token Formula
parseNonBinOp =
  parseEmpty
  |> or parseBottom
  |> or parseNegation
  |> or parseEquality
  |> or parseInequality
  |> or parseParenthesized
  |> or parseApplication
  |> or parseDeclaration
  |> or parseForall
  |> or parseExists

parseBinOpWithFallthrough : tok -> Parser tok res -> (res -> res -> res) -> Parser tok res
parseBinOpWithFallthrough opToken innerParser makeResult =
  with innerParser (\lhs ->
  with (peek 1) (\operator ->
  if operator /= [opToken]
  then return lhs
  else takeOne
       |> kThen (with innerParser <| \rhs ->
       return (makeResult lhs rhs))))

parseConjunction = parseBinOpWithFallthrough TokAnd parseNonBinOp Conjunction
parseDisjunction = parseBinOpWithFallthrough TokOr parseConjunction Disjunction
parseImplication = parseBinOpWithFallthrough TokIf parseDisjunction Implication
parseBiconditional = parseBinOpWithFallthrough TokIff parseImplication Biconditional

parseTop = parseBiconditional

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

prettifyText : Bool -> String -> String
prettifyText useUnicode = tokenize >> renderTokens >> TextStyle.map useUnicode

pretty : Formula -> String
pretty formula = case formula of
  Empty -> ""
  Bottom -> "⊥"
  Name name -> String.fromChar name
  Declaration name -> "[" ++ String.fromChar name ++ "]"
  Application name args -> String.fromChar name ++ String.join "" (List.map String.fromChar args)
  Negation body -> "¬(" ++ pretty body ++ ")"
  Conjunction lhs rhs -> "(" ++ pretty lhs ++ ")∧(" ++ pretty rhs ++ ")"
  Disjunction lhs rhs -> "(" ++ pretty lhs ++ ")∨(" ++ pretty rhs ++ ")"
  Implication lhs rhs -> "(" ++ pretty lhs ++ ")→(" ++ pretty rhs ++ ")"
  Biconditional lhs rhs -> "(" ++ pretty lhs ++ ")↔(" ++ pretty rhs ++ ")"
  Forall name body -> "∀" ++ String.fromChar name ++ "(" ++ pretty body ++ ")"
  Exists name body -> "∃" ++ String.fromChar name ++ "(" ++ pretty body ++ ")"
  Equality lhs rhs -> String.fromChar lhs ++ "=" ++ String.fromChar rhs

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

declaring : Formula -> Set Char
declaring formula = case formula of
  Declaration name -> Set.singleton name
  Forall name _ -> Set.singleton name
  Exists name _ -> Set.singleton name
  _ -> Set.empty

children : Formula -> List Formula
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

