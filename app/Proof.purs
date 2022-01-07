module Fitch.Proof where

import Prelude
import Prelude as Prelude
import Data.Maybe (Maybe (..))
import Data.Array as Array

import Fitch.Types (Proofy(..), Path)
import Fitch.Util.ArrayUtil as ArrayUtil
import Fitch.Util.MaybeUtil as MaybeUtil

get :: forall a. Path -> Proofy a -> Maybe (Proofy a)
get path proof = case Array.uncons path of
  Nothing -> Just proof
  Just { head: idx, tail: idxs } ->
    case proof of
      ProofLine _ -> Nothing
      ProofBlock head body ->
        if idx >= 0
        then Array.index body idx
             # (_ >>= get idxs)
        else Array.index head (-idx-1)
             # Prelude.map (\formula -> ProofLine formula)
             # (_ >>= (\subproof -> get idxs subproof))

set :: forall a. Int -> Proofy a -> Proofy a -> Maybe (Proofy a)
set idx subproof proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then case subproof of
        ProofLine line -> Array.updateAt idx line head
                          # Prelude.map (\newHead -> ProofBlock newHead body)
        ProofBlock _ _ -> Nothing
      else Array.updateAt (-idx-1) subproof body
           # Prelude.map (\newBody -> ProofBlock head newBody)

remove :: forall a. Int -> Proofy a -> Maybe (Proofy a)
remove idx proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then Array.deleteAt idx body # Prelude.map (\newBody -> ProofBlock head newBody)
      else Array.deleteAt (-idx-1) head # Prelude.map (\newHead -> ProofBlock newHead body)

firstLine :: forall a. Proofy a -> Maybe a
firstLine proof = case proof of
  ProofLine line -> Just line
  ProofBlock head body ->
    Array.head head
    # MaybeUtil.orElseLazy (\_ -> Array.head body >>= firstLine)

lastLine :: forall a. Proofy a -> Maybe a
lastLine proof = case proof of
  ProofLine line -> Just line
  ProofBlock head body ->
    Array.last body
    # (_ >>= lastLine)
    # MaybeUtil.orElseLazy (\_ -> Array.last head)

assumptions :: forall a. Proofy a -> Array a
assumptions proof = case proof of
  ProofLine _ -> []
  ProofBlock head _ -> head

conclusion :: forall a. Proofy a -> Maybe a
conclusion proof = case proof of
  ProofLine _ -> Nothing
  ProofBlock _ _ -> lastLine proof

map :: forall a b. (a -> b) -> Proofy a -> Proofy b
map fn proof = case proof of
  ProofLine line -> ProofLine (fn line)
  ProofBlock head body -> ProofBlock (Prelude.map fn head) (Prelude.map (map fn) body)

replaceM :: forall a. Int -> (Proofy a -> Maybe (Proofy a)) -> Proofy a -> Maybe (Proofy a)
replaceM idx fn proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then
        Array.index body idx
        # (_ >>= fn)
        # (_ >>= (\newSubproof -> Array.updateAt idx newSubproof body))
        # Prelude.map (\newBody -> ProofBlock head newBody)
      else
        Array.index head (-idx-1)
        # Prelude.map ProofLine
        # (_ >>= fn)
        # (_ >>= (\newSubproof -> case newSubproof of
          ProofLine newLine -> Array.updateAt (-idx-1) newLine head
          ProofBlock _ _ -> Nothing))
        # Prelude.map (\newHead -> ProofBlock newHead body)

replace :: forall a. Int -> (Proofy a -> Proofy a) -> Proofy a -> Maybe (Proofy a)
replace idx fn = replaceM idx (fn >>> Just)

length :: forall a. Proofy a -> Int
length proof = case proof of
  ProofLine _ -> 1
  ProofBlock head body -> Array.length head + Array.length body
