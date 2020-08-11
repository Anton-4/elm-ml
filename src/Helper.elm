module Helper exposing (nxt, nxtWithArg, listToString, arrToString)

import Array exposing (Array)

nxt : (a -> Result e b) -> Result e a -> Result e b
nxt callback result =
    case result of
      Ok value -> callback value
      Err msg -> Err msg

nxtWithArg : (a -> x -> Result e b) -> x -> Result e a -> Result e b
nxtWithArg callback arg result =
    case result of
      Ok value -> callback value arg
      Err msg -> Err msg

listToString : List Int -> String
listToString lst =
  List.map String.fromInt lst
  |> String.join ", "

arrToString : Array Int -> String
arrToString arr =
  listToString <| Array.toList arr
