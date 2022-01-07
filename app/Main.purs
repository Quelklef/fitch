module Main where

import Prelude
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Platform (Update, app)
import Data.Maybe (fromMaybe)

import Fitch.Types (Model, Proofy(..))
import Fitch.Update (update)
import Fitch.View (view)
import Fitch.Serialize as Serialize

foreign import getUrlArg :: Effect String

main :: Effect Unit
main = do
  let fn = app
        { init
        , subscriptions: const mempty
        , update: flip update
        , view
        }

  arg <- getUrlArg
  runEffectFn1 fn arg

init :: forall msg. String -> Update msg Model
init proofFromUrl =
  proofFromUrl
  # Serialize.deserialize
  # fromMaybe (ProofBlock [""] [])
  # (\proof -> { proof, showDebugInfo: false, useUnicode: true })
  # pure
