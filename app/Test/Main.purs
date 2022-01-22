module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Test.Edge as E
import Test.Property as P

main :: Effect Unit
main = do

  suite E.main "Edge cases"
  suite P.main "Property tests"


  where

  suite m label = do
    log ""
    log label
    m
    log "done"
