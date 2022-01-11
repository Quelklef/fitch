module Fitch.Proof where

import Prelude
import Prelude as Prelude
import Data.Maybe (Maybe (..))
import Data.Array as Array
import Control.Alt ((<|>))

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
        then get idxs =<< Array.index body idx
        else get idxs =<< (ProofLine <$> Array.index head (-idx-1))

set :: forall a. Int -> Proofy a -> Proofy a -> Maybe (Proofy a)
set idx subproof proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then case subproof of
        ProofLine line -> Array.updateAt idx line head
                          # map (\newHead -> ProofBlock newHead body)
        ProofBlock _ _ -> Nothing
      else Array.updateAt (-idx-1) subproof body
           # map (\newBody -> ProofBlock head newBody)

remove :: forall a. Int -> Proofy a -> Maybe (Proofy a)
remove idx proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then Array.deleteAt idx body # map (\newBody -> ProofBlock head newBody)
      else Array.deleteAt (-idx-1) head # map (\newHead -> ProofBlock newHead body)

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
    (Array.last body >>= lastLine) <|> Array.last head

assumptions :: forall a. Proofy a -> Array a
assumptions proof = case proof of
  ProofLine _ -> []
  ProofBlock head _ -> head

conclusion :: forall a. Proofy a -> Maybe a
conclusion proof = case proof of
  ProofLine _ -> Nothing
  ProofBlock _ _ -> lastLine proof

replaceM :: forall a. Int -> (Proofy a -> Maybe (Proofy a)) -> Proofy a -> Maybe (Proofy a)
replaceM idx fn proof =
  case proof of
    ProofLine _ -> Nothing
    ProofBlock head body ->
      if idx >= 0 then do
        newSubproof <- fn =<< Array.index body idx
        newBody <- Array.updateAt idx newSubproof body
        pure $ ProofBlock head newBody
      else do
        newSubproof <- fn =<< (ProofLine <$> Array.index head (-idx-1))
        newHead <- case newSubproof of
          ProofLine newLine -> Array.updateAt (-idx-1) newLine head
          ProofBlock _ _ -> Nothing
        pure $ ProofBlock newHead body

replace :: forall a. Int -> (Proofy a -> Proofy a) -> Proofy a -> Maybe (Proofy a)
replace idx fn = replaceM idx (fn >>> Just)

length :: forall a. Proofy a -> Int
length = case _ of
  ProofLine _ -> 1
  ProofBlock head body -> Array.length head + Array.length body
