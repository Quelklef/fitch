module Main where

import Prelude
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Platform (app)
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Either (hush)

import Fitch.Types (Proofy(..))
import Fitch.Update (update)
import Fitch.View (view)
import Fitch.Serialize as Serialize

foreign import getUrlArg :: Effect (Nullable String)

main :: Effect Unit
main = do

  arg <- Nullable.toMaybe <$> getUrlArg
  let proof0 = arg >>= (Serialize.deserialize >>> hush) # fromMaybe (ProofBlock [""] [])
  let model0 = { proof: proof0, showDebugInfo: false }

  let fn = app
        { init: const $ pure model0
        , subscriptions: const mempty
        , update: flip update
        , view
        }

  runEffectFn1 fn unit
