module Main where

import Prelude
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Platform (app)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Either (hush)
import Control.Monad.Trans.Class (lift)

import Fitch.Types (Model, nilModel)
import Fitch.Update (update)
import Fitch.View (view)
import Fitch.Serialize as Serialize
import Fitch.SyncUrl as SyncUrl

main :: Effect Unit
main =
  flip runEffectFn1 unit $ app
    { init
    , subscriptions: const mempty
    , update: flip update
    , view
    }

  where

  init _ = do
    params <- lift SyncUrl.readParams
    let model0 = fromUrlParams params # fromMaybe nilModel
    pure model0

fromUrlParams :: Map String String -> Maybe Model
fromUrlParams params = do
  strictNames <-
    Map.lookup "names" params
    >>= case _ of
          "strict" -> Just true
          "lax" -> Just false
          _ -> Nothing

  proof <- Map.lookup "proof" params >>= (Serialize.deserialize >>> hush)

  pure { proof, strictNames, showDebugInfo: false }
