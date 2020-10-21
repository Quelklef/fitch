module Parse exposing (..)

import ListUtil
import MaybeUtil

-- Because Elm's built-in parser doesn't seem to allow for working on token lists instead of strings

type alias Parser tok res = List tok -> Maybe (res, List tok)

or : Parser tok res -> Parser tok res -> Parser tok res
or snd fst tokens = fst tokens |> MaybeUtil.orElseLazy (\() -> snd tokens)

kThen : Parser tok res -> Parser tok ignored -> Parser tok res
kThen snd fst tokens = fst tokens |> Maybe.andThen (\(result, rest) -> snd rest)

withM : Parser tok a -> (a -> Maybe (Parser tok b)) -> Parser tok b
withM valParser getParser =
  valParser >> Maybe.andThen (\(val, rest) ->
      getParser val
      |> Maybe.andThen (\gotParser -> gotParser rest))

with : Parser tok a -> (a -> Parser tok b) -> Parser tok b
with valParser getParser = withM valParser (getParser >> Just)

lazy : (() -> Parser tok a) -> Parser tok a
lazy getParser tokens = (getParser ()) tokens

literal : List tok -> Parser tok (List tok)
literal lit tokens =
  if ListUtil.startsWith lit tokens
  then Just (lit, List.drop (List.length lit) tokens)
  else Nothing

return : res -> Parser tok res
return val tokens = Just (val, tokens)

zeroPlus : Parser tok res -> Parser tok (List res)
zeroPlus parser tokens =
  tokens |> with parser
    (\head -> zeroPlus parser >> Maybe.map (\(tail, rest) -> (head :: tail, rest)))
    |> Maybe.withDefault ([], tokens) >> Just

mapResultsM : (a -> Maybe b) -> Parser tok a -> Parser tok b
mapResultsM mapper parser = parser >> Maybe.andThen (\(val, rest) -> mapper val |> Maybe.map (\newVal -> (newVal, rest)))

mapResults : (a -> b) -> Parser tok a -> Parser tok b
mapResults mapper parser = parser >> Maybe.map (\(val, rest) -> (mapper val, rest))

onePlus : Parser tok res -> Parser tok (List res)
onePlus parser = zeroPlus parser |> mapResultsM (\results -> if List.length results == 0 then Nothing else Just results)

takeOne : Parser tok tok
takeOne tokens = case tokens of
  [] -> Nothing
  tok::rest -> Just (tok, rest)

peekOne : Parser tok (Maybe tok)
peekOne tokens =
  let seen = case tokens of
        [] -> Nothing
        tok::rest -> Just tok
  in Just (seen, tokens)

eof : Parser tok ()
eof tokens = case tokens of
  [] -> Just ((), [])
  _ -> Nothing
