module Fitch.Path where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Foldable (intercalate)

import Fitch.Types (Proofy (..), Path)
import Fitch.Proof as Proof
import Fitch.Util.ArrayUtil as ArrayUtil
import Fitch.Util.MaybeUtil as MaybeUtil

pretty :: Path -> String
pretty = map show >>> intercalate " → "

pathToLastLine :: forall a. Proofy a -> Maybe Path
pathToLastLine proof = case proof of
  ProofLine _ -> Just []
  ProofBlock head body -> do
    idx <-
      if Array.length body > 0 then Just $ Array.length body - 1
      else if Array.length head > 0 then Just $ -(Array.length head)
      else Nothing
    subproof <- Proof.get [idx] proof
    pathTail <- pathToLastLine subproof
    pure $ Array.cons idx pathTail

pathToFirstLine :: forall a. Proofy a -> Maybe Path
pathToFirstLine proof = case proof of
  ProofLine _ -> Just []
  ProofBlock head body -> do
    idx <-
      if Array.length head > 0 then Just $ -(Array.length head)
      else if Array.length body > 0 then Just 0
      else Nothing
    subproof <- Proof.get [idx] proof
    pathTail <- pathToFirstLine subproof
    pure $ Array.cons idx pathTail

indexOfLastAssumption :: forall a. Proofy a -> Maybe Int
indexOfLastAssumption proof = case proof of
  ProofLine _ -> Nothing
  ProofBlock head _ -> if Array.length head == 0 then Nothing else Just $ -(Array.length head)

-- ↓ Given an index targeting a line, does the following:
-- ↓ Given a path in a proof that targets a line, evaluates the
-- ↓ path which targets the "linearly next" line, ignoring nesting.
-- ↓ For instance, consider the following:
-- ↓   ProofBlock
-- ↓     [ "line 1"
-- ↓     , "line 2" ]
-- ↓     [ ProofLine "line 3"
-- ↓     , ProofBlock
-- ↓         [ "line 4" ]
-- ↓         [ ProofLine "line 5" ]
-- ↓     , ProofLine "line 6" ]
-- ↓ The above example names the lines as per their linear order.
-- ↓ Thus the linear successor of the path [-1], which targets "line 1",
-- ↓ is [-2], which targets "line 2"; the linear successor of that
-- ↓ is [0], targeting "line 3", then [1, -1], targeting "line 4",
-- ↓ then [1, 0], targeting "line 5", and finally [2], targeting "line 6"
linearSucc :: forall a. Proofy a -> Path -> Maybe Path
linearSucc proof = Array.uncons >>> case _ of
  Nothing -> Nothing
  Just { head: idx, tail: idxs } ->
    let
        -- ↓ Attempt to look for the result at the current idx
        hereAttempt = do
          subproof <- Proof.get [idx] proof
          pathTail <- linearSucc subproof idxs
          pure $ Array.cons idx pathTail

        -- ↓ If that fails, we'll look for the result in the succeeding index
        succAttempt _ = do
          let succIdx =
                if targetsLastAssumption proof [idx] then 0
                else if idx < 0 then idx - 1
                else idx + 1
          subproof <- Proof.get [succIdx] proof
          case subproof of
             ProofLine _ -> Just [succIdx]
             ProofBlock _ _ -> Array.cons succIdx <$> pathToFirstLine subproof

    in hereAttempt # MaybeUtil.orElseLazy succAttempt

-- ↓ Like linearSucc, but in the opposite direction
linearPred :: forall a. Proofy a -> Path -> Maybe Path
linearPred proof = Array.uncons >>> case _ of
  Nothing -> Nothing
  Just { head: idx, tail: idxs } ->
    let
        hereAttempt = do
          subproof <- Proof.get [idx] proof
          pathTail <- linearPred subproof idxs
          pure $ Array.cons idx pathTail

        predAttempt _ = do
          predIdx <-
            if idx == 0 then indexOfLastAssumption proof
            else if idx == -1 then Nothing
            else if idx < 0 then Just $ idx + 1
            else Just $ idx - 1
          subproof <- Proof.get [predIdx] proof
          case subproof of
             ProofLine _ -> Just [predIdx]
             ProofBlock _ _ -> Array.cons predIdx <$> pathToLastLine subproof

    in hereAttempt # MaybeUtil.orElseLazy predAttempt

toId :: Path -> String
toId path = "path_" <> intercalate "_" (show <$> path)

targetsFirstAssumption :: forall a. Proofy a -> Path -> Boolean
targetsFirstAssumption proof = Array.uncons >>> case _ of
  Nothing -> false
  Just { head: idx, tail: [] } -> case proof of
    ProofLine _ -> false
    ProofBlock head _body -> idx == (-(Array.length head))
  Just { head: idx, tail: idxs } ->
    Proof.get [idx] proof
    # map (\subproof -> targetsFirstAssumption subproof idxs)
    # fromMaybe false

targetsLastAssumption :: forall a. Proofy a -> Path -> Boolean
targetsLastAssumption proof = Array.uncons >>> case _ of
  Nothing -> false
  Just { head: idx, tail: [] } -> case proof of
    ProofLine _ -> false
    ProofBlock head _ -> idx == -(Array.length head)
  Just { head: idx, tail: idxs } ->
    Proof.get [idx] proof
    # map (\subproof -> targetsLastAssumption subproof idxs)
    # fromMaybe false

targetsLastLine :: forall a. Proofy a -> Path -> Boolean
targetsLastLine proof = Array.uncons >>> case _ of
  Nothing -> false
  Just { head: idx, tail: [] } -> case proof of
    ProofLine _ -> false
    ProofBlock head body ->
      Array.length body == 0 && idx == -(Array.length head)
      || idx == Array.length body - 1
  Just { head: idx, tail: idxs } ->
    Proof.get [idx] proof
    # map (\subproof -> targetsLastLine subproof idxs)
    # fromMaybe false

targetsEmptyLine :: Proofy String -> Path -> Boolean
targetsEmptyLine proof path = case Proof.get path proof of
  Just (ProofLine "") -> true
  _ -> false

-- ↓ If the given path targets a ProofLine, rather than a ProofBlock, then
-- ↓ inserts a new line after that targeted line.
-- ↓ If the targeted line is in the proof body, the inserted line will also
-- ↓ be a body line.
-- ↓ If the targeted line is an assumption (ie in the proof head) and is
-- ↓ followed by another assumption, then the inserted line will also
-- ↓ be an assumption.
-- ↓ If the targeted line is an assumption and is *not* followed by another
-- ↓ assumption (ie it's the last assumption), then the inserted line will
-- ↓ be an assumption if and only if `preferAssumption` is true; otherwise,
-- ↓ it will be a body line.
insertAfter :: forall a. Boolean -> Path -> a -> Proofy a -> Maybe (Proofy a)
insertAfter preferAssumption path line host =
  case Array.uncons path of
    Nothing -> Nothing

    Just { head: idx, tail: [] } ->
      case host of
        ProofLine _ -> Nothing
        ProofBlock head body ->
          -- ↓ Index targets last assumption
          if targetsLastAssumption host [idx] then
            if preferAssumption
            then Just $ ProofBlock (head <> [line]) body
            else Just $ ProofBlock head (Array.cons (ProofLine line) body)
          -- ↓ Index targets an assumption that is not the last one
          else if idx < 0 then
            Array.insertAt (-idx-1 + 1) line head
            # map (\newHead -> ProofBlock newHead body)
          -- ↓ Index targets a body line
          else
            Array.insertAt (idx + 1) (ProofLine line) body
            # map (\newBody -> ProofBlock head newBody)

    Just { head: idx, tail: idxs } ->
        Proof.replaceM idx (insertAfter preferAssumption idxs line) host
