module Fitch.Update where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Array as Array
import Data.Tuple.Nested ((/\), type (/\))
import Control.Monad.Writer.Class (tell)
import Task (makeTask)

import Platform (Update, Cmd, attemptTask)

import Fitch.Types (Model, Message (..), Path, Proofy (..))
import Fitch.Path as Path
import Fitch.Proof as Proof
import Fitch.Decorate as Decorate
import Fitch.Serialize as Serialize

-- ↓ Replaces the url of the page without reloading
foreign import setUrlArg :: String -> Effect Unit

foreign import copyToClipboard :: String -> Effect Unit

foreign import getByIdAndSetFocus :: String -> Effect Unit

effectToCmd :: forall a. Effect a -> Cmd Message
effectToCmd eff = attemptTask (const Noop) task
  where task = makeTask \done _ -> eff *> done unit *> pure (pure unit)

setFocusTo :: Path -> Cmd Message
setFocusTo path = effectToCmd (getByIdAndSetFocus $ Path.toId path)

update :: Message -> Model -> Update Message Model
update msg model =
  case msg of
    ToggleDebugMode      -> pure $ model { showDebugInfo = not model.showDebugInfo }
    ToggleUseUnicode     -> pure $ model { useUnicode = not model.useUnicode }
    CopyProofToClipboard -> model <$ (tell <<< effectToCmd) (model.proof # Decorate.decorate # Decorate.toText model.useUnicode # copyToClipboard)
    Noop                 -> pure model
    SetFocusTo path  -> model <$ tell (setFocusTo path)
    SetProofTo newProof                -> fromNewProofAndCommand $ Just (newProof /\ mempty)
    SetFormulaAt path newFormula       -> fromNewProofAndCommand $ doSetFormulaAt path newFormula model.proof
    NewLineAfter path preferAssumption -> fromNewProofAndCommand $ doNewLineAfter path preferAssumption model.proof
    IndentAt path                      -> fromNewProofAndCommand $ doIndentAt path model.proof
    DedentAt path                      -> fromNewProofAndCommand $ doDedentAt path model.proof
    BackspaceAt path                   -> fromNewProofAndCommand $ doBackspaceAt path model.proof

  where

  fromNewProofAndCommand = case _ of
     Just (newProof /\ cmd) -> do
       tell cmd
       (tell <<< effectToCmd) (setUrlArg $ Serialize.serialize newProof)
       pure $ model { proof = newProof }
     Nothing -> pure model

doSetFormulaAt :: Path -> String -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doSetFormulaAt path newFormula proof = (_ /\ mempty) <$> doSetFormulaAtImpl path proof
  where

  doSetFormulaAtImpl :: Path -> Proofy String -> Maybe (Proofy String)
  doSetFormulaAtImpl path proof =
    case Array.uncons path of
      Nothing -> case proof of
        ProofBlock _ _ -> Nothing
        ProofLine  _oldFormula -> Just (ProofLine newFormula)
      Just { head: idx, tail: idxs } -> Proof.replaceM idx (doSetFormulaAtImpl idxs) proof

doNewLineAfter :: Path -> Boolean -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doNewLineAfter path preferAssumption proof = do
  newProof <- Path.insertAfter preferAssumption path "" proof
  newPath <- Path.linearSucc newProof path
  pure $ newProof /\ setFocusTo newPath

doIndentAt :: Path -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doIndentAt path proof = do
  newProof <- doIndentAtImpl path proof
  pure $ newProof /\ setFocusTo (path <> [-0-1])
  where

  doIndentAtImpl :: Path -> Proofy String -> Maybe (Proofy String)
  doIndentAtImpl path proof =
    case Array.uncons path of
      Nothing -> case proof of
        ProofLine line -> Just $ ProofBlock [line] []
        ProofBlock _ _ -> Nothing
      Just { head: idx, tail: idxs } -> Proof.replaceM idx (doIndentAtImpl idxs) proof

doDedentAt :: Path -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doDedentAt path proof = do
  newProof <- doDedentAtImpl path proof
  let targetHasOnlyOneLine =
          fromMaybe false do
            block <- Proof.get (Array.dropEnd 1 path) proof
            pure $ Proof.length block == 1
  newPath <-
    if targetHasOnlyOneLine
    then Just $ Array.dropEnd 1 path
    else path # Path.linearPred proof >>= Path.linearSucc newProof
  pure $ newProof /\ setFocusTo newPath

  where

  doDedentAtImpl :: Path -> Proofy String -> Maybe (Proofy String)
  doDedentAtImpl path proof =
    case Array.uncons path of
      Nothing -> Nothing
      Just { head: _, tail: [] } -> Nothing

      Just { head: parentIdx, tail: [childIdx] } -> case proof of
        ProofLine _ -> Nothing
        ProofBlock _ _ ->
          let parent = proof
              maybeChild = Proof.get [parentIdx] parent
              maybeChildLastLine = maybeChild >>= Proof.lastLine
          in case maybeChild /\ maybeChildLastLine of
            Just child@(ProofBlock childHead childBody) /\ Just childLastLine ->
               let childHasOnlyOneLine = Array.length childHead + Array.length childBody == 1
                   targetsLastLine = Path.targetsLastLine child [childIdx]
               -- ↓ Allow dedenting a line only if it targets the last line of the proof
               in if not targetsLastLine then Nothing
                  -- ↓ If only one line, collapse it down into parent proof
                  else if childHasOnlyOneLine then Proof.replace parentIdx (const $ ProofLine childLastLine) parent
                  -- ↓ If more than one line, need to collapse the last line
                  -- ↓ down but leave the rest of the block intact
                  else let newChild = Proof.remove childIdx child
                       in parent
                          # Proof.replaceM parentIdx (const newChild)
                          >>= Path.insertAfter false [parentIdx] childLastLine
            _ -> Nothing

      Just { head: idx, tail: idxs } -> Proof.replaceM idx (doDedentAtImpl idxs) proof

doBackspaceAt :: Path -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doBackspaceAt path proof =
  if not $ Path.targetsEmptyLine proof path
  then Nothing
  else do newProof <- doBackspaceAtImpl path proof
          newPath <- Path.linearPred proof path
          pure $ newProof /\ setFocusTo newPath
  where

  doBackspaceAtImpl :: Path -> Proofy String -> Maybe (Proofy String)
  doBackspaceAtImpl path proof = case Array.uncons path of
    Nothing -> Nothing
    Just { head: idx, tail: [] } -> case proof of
      ProofLine _ -> Nothing
      ProofBlock head body ->
              if idx < 0 then
                  -- ↓ Disallow backspacing a lonely assumption on a nonempty body
                  if Array.length head == 1 && Array.length body > 0
                  then Nothing
                  else Array.deleteAt (-idx-1) head <#> (\newHead -> ProofBlock newHead body)
              else (Array.deleteAt idx body) <#> (\newBody -> ProofBlock head newBody)
    Just { head: idx, tail: idxs } -> do
      newProof <- proof # Proof.replaceM idx (doBackspaceAtImpl idxs)
      -- ↓ Don't leave behind an empty block
      case Proof.get [idx] newProof of
        Just (ProofBlock head body) ->
          if Array.length head == 0 && Array.length body == 0
          then Proof.remove idx newProof
          else Just newProof
        _ -> Just newProof
