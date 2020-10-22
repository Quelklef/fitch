module Types exposing (..)

import Proof exposing (RawProof)
import Path exposing (Path)

type alias Model =
  { proof : RawProof
  , showDebugInfo : Bool
  , useUnicode : Bool
  }

type Message
  = ToggleDebugMode
  | ToggleUseUnicode
  | SetProofTo RawProof
  | Noop
  | SetFocusTo Path
  | SetFormulaAt Path String
  | NewLineAfter Path Bool
  | IndentAt Path
  | DedentAt Path
  | BackspaceAt Path
