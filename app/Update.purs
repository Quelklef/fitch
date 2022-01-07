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
  let fromNewProofAndCommand maybeProofAndCommand = case maybeProofAndCommand of
        Just (newProof /\ cmd) -> do
          tell cmd
          (tell <<< effectToCmd) (setUrlArg $ Serialize.serialize newProof)
          pure $ model { proof = newProof }
        Nothing -> pure model
  in case msg of
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

doSetFormulaAt :: Path -> String -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doSetFormulaAt path newFormula proof =
  doSetFormulaAt_ path newFormula proof
  <#> (\newProof -> (newProof /\ mempty))

doSetFormulaAt_ :: Path -> String -> Proofy String -> Maybe (Proofy String)
doSetFormulaAt_ path newFormula proof =
  case Array.uncons path of
    Nothing -> case proof of
      ProofBlock _ _ -> Nothing
      ProofLine  _oldFormula -> Just (ProofLine newFormula)
    Just { head: idx, tail: idxs } -> Proof.replaceM idx (\subproof -> doSetFormulaAt_ idxs newFormula subproof) proof

doNewLineAfter :: Path -> Boolean -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doNewLineAfter path preferAssumption proof =
  doNewLineAfter_ path preferAssumption proof
  >>= (\newProof ->
    Path.linearSucc newProof path
    <#> (\newPath -> newProof /\ setFocusTo newPath))

doNewLineAfter_ :: Path -> Boolean -> Proofy String -> Maybe (Proofy String)
doNewLineAfter_ path preferAssumption proof = Path.insertAfter preferAssumption path "" proof

doIndentAt :: Path -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doIndentAt path proof =
  doIndentAt_ path proof
  <#> (\newProof -> newProof /\ (setFocusTo $ path <> [-0-1]))

doIndentAt_ :: Path -> Proofy String -> Maybe (Proofy String)
doIndentAt_ path proof =
  case Array.uncons path of
    Nothing -> case proof of
      ProofLine line -> Just $ ProofBlock [line] []
      ProofBlock _ _ -> Nothing
    Just { head: idx, tail: idxs } -> Proof.replaceM idx (\subproof -> doIndentAt_ idxs subproof) proof

doDedentAt :: Path -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doDedentAt path proof =
  doDedentAt_ path proof
  >>= (\newProof ->
    let targetHasOnlyOneLine =
          Array.dropEnd 1 path
          # (\thePath -> Proof.get thePath proof)
          # map (\block -> Proof.length block == 1)
          # fromMaybe false
        maybeNewPath =
          if targetHasOnlyOneLine
          then Array.dropEnd 1 path # Just
          else path # Path.linearPred proof >>= Path.linearSucc newProof
    in maybeNewPath
       <#> (\newPath -> newProof /\ setFocusTo newPath))

doDedentAt_ :: Path -> Proofy String -> Maybe (Proofy String)
doDedentAt_ path proof =
  case Array.uncons path of
    Nothing -> Nothing
    Just { head: _, tail: [] } -> Nothing

    Just { head: parentIdx, tail: [childIdx] } -> case proof of
      ProofLine _ -> Nothing
      ProofBlock _parentHead _parentBody ->
        let parent = proof
            maybeChild = Proof.get [parentIdx] parent
            maybeChildLastLine = maybeChild >>= Proof.lastLine
        in case maybeChild /\ maybeChildLastLine of
          Just (ProofBlock childHead childBody) /\ Just childLastLine ->
             let child = ProofBlock childHead childBody
                 childHasOnlyOneLine = Array.length childHead + Array.length childBody == 1
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
                        >>= (Path.insertAfter false [parentIdx] childLastLine)
          _ -> Nothing

    Just { head: idx, tail: idxs } -> Proof.replaceM idx (\subproof -> doDedentAt_ idxs subproof) proof

doBackspaceAt :: Path -> Proofy String -> Maybe (Proofy String /\ Cmd Message)
doBackspaceAt path proof =
  if not $ Path.targetsEmptyLine proof path
    then Nothing
    else let maybeNewProof = doBackspaceAt_ path proof
             maybeNewPath = Path.linearPred proof path
         in (\newProof newPath -> newProof /\ setFocusTo newPath) <$> maybeNewProof <*> maybeNewPath

doBackspaceAt_ :: Path -> Proofy String -> Maybe (Proofy String)
doBackspaceAt_ path proof = case Array.uncons path of
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
  Just { head: idx, tail: idxs } ->
    proof
    # Proof.replaceM idx (\subproof -> doBackspaceAt_ idxs subproof)
    -- ↓ Don't leave behind an empty block
    >>= (\newProof -> case Proof.get [idx] newProof of
      Just (ProofBlock head body) ->
        if Array.length head == 0 && Array.length body == 0
        then Proof.remove idx newProof
        else Just newProof
      _ -> Just newProof)
