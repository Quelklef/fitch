module Path exposing (..)

import Maybe exposing (Maybe)

-- vv Path to a formula in a proof, as a list of indicies
-- vv A negative index is an index into a block head; positive into the body
type alias Path = List Int

next : Path -> Maybe Path
next path = case path of
  [] -> Nothing
  [idx] ->
    let newIdx = if idx >= 0 then idx + 1 else idx - 1
    in Just [newIdx]
  idx::idxs -> next idxs |> Maybe.map (\tail -> idx :: tail)

toId : Path -> String
toId path = "path_" ++ String.join "_" (List.map String.fromInt path)

