module Fitch.SyncUrl where

import Prelude
import Effect (Effect)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))


readParams :: Effect (Map String String)
readParams = Map.fromFoldable <$> getParams_ar
  where

  getParams_ar :: Effect (Array (String /\ String))
  getParams_ar = asm """ () => {
    const params = new URL(window.location.href).searchParams;
    return [...params.keys()].map(k => #{(/\)}(k)(params.get(k)));
  } """


-- Use an association list to be order-preserving
writeParams :: (Array (String /\ String)) -> Effect Unit
writeParams =
  asm """ kvs => () => {
    const params = new URLSearchParams();
    for (const kv of kvs) params.append(#{fst}(kv), #{snd}(kv));
    window.history.replaceState(null, '', window.location.pathname + '?' + params.toString());
  } """
