module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe (..), fromJust)
import Data.Either (Either (..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (intercalate)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (Result, (<?>), quickCheck')
import Test.QuickCheck.Gen (Gen, suchThat, chooseInt)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

import Fitch.Path as Path
import Fitch.Path (linearSucc, linearPred)
import Fitch.Types (Path, Proofy (..))
import Fitch.Serialize (serialize, deserialize)
import Fitch.Serialize as Serialize

main :: Effect Unit
main = do

  log "serialize >> deserialize = id"
  quickCheck' 25 testSerializationRoundtrip

  log "path succ >> pred = id"
  quickCheck' 300 $ testLinearStepUnstep Path.pathToLastLine linearSucc linearPred

  log "path pred >> succ = id"
  quickCheck' 300 $ testLinearStepUnstep Path.pathToFirstLine linearPred linearSucc


testSerializationRoundtrip :: Proofy String -> Result
testSerializationRoundtrip =
  \pf ->
    let ser = serialize pf
        des = deserialize ser
    in des == Right pf
       <?> (intercalate "\n"
             [ "Test failed."
             , "Proof: " <> show pf
             , "Serialized: " <> show ser
             , "Decoded: " <> Serialize.fromPayload ser
             , "Deserialized: " <> show des
             ])


testLinearStepUnstep
  :: (Proofy Unit -> Maybe Path)
  -> (Proofy Unit -> Path -> Maybe Path)
  -> (Proofy Unit -> Path -> Maybe Path)
  -> Gen Result
testLinearStepUnstep getOmittedPath step unstep  =

    test <$> (genProofsAndPaths' `suchThat` (not <<< pathIsOmitted))

  where

  pathIsOmitted (proof /\ path) = getOmittedPath proof == Just path
  test (proof /\ path) =

    (step proof >=> unstep proof) path == Just path
    <?> (intercalate "\n"
          [ "Test failed."
          , "Raw proof: " <> show proof
          , "Proof url: http://localhost:8000/?proof=" <> serialize (proof $> "")
          , "Path: " <> (intercalate " → " $ show <$> path)
          ])


  genProofsAndPaths' :: Gen (Proofy Unit /\ Path)
  genProofsAndPaths' = genProofsAndPaths

  genProofsAndPaths :: forall ln. Arbitrary ln => Gen (Proofy ln /\ Path)
  genProofsAndPaths = do
    proof <- arbitrary
    path <- genPath proof
    pure $ proof /\ path

    where

    genPath :: forall a. Proofy a -> Gen Path
    genPath = case _ of
      ProofLine _ -> pure []
      ProofBlock hd bd -> do
        idx <- chooseInt (-(Array.length hd - 1)-1) (Array.length bd - 1)
        if idx < 0
        then pure [idx]
        else let subproof = unsafeFromJust (bd !! idx)
             in Array.cons idx <$> genPath subproof

    unsafeFromJust :: forall a. Maybe a -> a
    unsafeFromJust x = unsafePartial (fromJust x)
