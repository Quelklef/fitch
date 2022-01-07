module Fitch.Util.Parse where

import Prelude
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Array as Array

import Fitch.Util.ArrayUtil as ArrayUtil
import Fitch.Util.MaybeUtil as MaybeUtil

type Parser tok res = Array tok -> Maybe (res /\ Array tok)

or ::forall tok res.  Parser tok res -> Parser tok res -> Parser tok res
or snd fst tokens = fst tokens # MaybeUtil.orElseLazy (\_ -> snd tokens)

kThen :: forall tok res ignored. Parser tok res -> Parser tok ignored -> Parser tok res
kThen snd fst tokens = fst tokens >>= (\(_result /\ rest) -> snd rest)

withM :: forall tok a b. Parser tok a -> (a -> Maybe (Parser tok b)) -> Parser tok b
withM valParser getParser toks =
  valParser toks >>= (\(val /\ rest) ->
      getParser val >>= (_ $ rest))

with :: forall tok a b. Parser tok a -> (a -> Parser tok b) -> Parser tok b
with valParser getParser = withM valParser (getParser >>> Just)

lazy :: forall tok a. (Unit -> Parser tok a) -> Parser tok a
lazy getParser tokens = (getParser unit) tokens

literal :: forall tok. Eq tok => Array tok -> Parser tok (Array tok)
literal lit tokens =
  if ArrayUtil.startsWith lit tokens
  then Just (lit /\ Array.drop (Array.length lit) tokens)
  else Nothing

return :: forall tok res. res -> Parser tok res
return val tokens = Just (val /\ tokens)

zeroPlus :: forall tok res. Parser tok res -> Parser tok (Array res)
zeroPlus parser tokens =
  tokens # with parser
    (\head -> zeroPlus parser >>> map (\(tail /\ rest) -> Array.cons head tail /\ rest))
    # fromMaybe ([] /\ tokens) >>> Just

mapResultsM :: forall tok a b. (a -> Maybe b) -> Parser tok a -> Parser tok b
mapResultsM fn parser toks = parser toks >>= (\(val /\ rest) -> fn val # map (\newVal -> newVal /\ rest))

mapResults :: forall tok a b. (a -> b) -> Parser tok a -> Parser tok b
mapResults fn parser = parser >>> map (\(val /\ rest) -> fn val /\ rest)

onePlus :: forall tok res. Parser tok res -> Parser tok (Array res)
onePlus parser = zeroPlus parser # mapResultsM (\results -> if Array.length results == 0 then Nothing else Just results)

takeOne :: forall tok. Parser tok tok
takeOne = Array.uncons >>> case _ of
  Nothing -> Nothing
  Just { head, tail } -> Just $ head /\ tail

drop :: forall tok. Int -> Parser tok Unit
drop n tokens = Just $ unit /\ Array.drop n tokens

peek :: forall tok. Int -> Parser tok (Array tok)
peek n tokens = Just $ Array.take n tokens /\ tokens

eof :: forall tok. Parser tok Unit
eof tokens = case tokens of
  [] -> Just $ unit /\ []
  _ -> Nothing
