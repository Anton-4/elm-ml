module Helper exposing (arrToString, combineResList, listToString, nxt, nxtWithArg, chk, chkM, chkNxt, getRes, maximumBy)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra as ListX


chk : String -> Result String a -> Result String a
chk errStr res =
    case res of
        Ok val ->
            Ok val
        Err e ->
            Err <| errStr ++ ": " ++ e


chkNxt : String -> (a -> Result String b) -> Result String a -> Result String b
chkNxt errStr fun res =
    case res of
        Ok val ->
            fun val
        Err e ->
            Err <| errStr ++ ": " ++ e


chkM : String -> Maybe a -> Result String a
chkM errStr maybe =
    case maybe of
        Just val ->
            Ok val
        Nothing ->
            Err <| errStr


nxt : (a -> Result e b) -> Result e a -> Result e b
nxt callback result =
    case result of
        Ok value ->
            callback value

        Err msg ->
            Err msg


getRes : comparable -> Dict comparable v -> Result String v
getRes key dict =
    case Dict.get key dict of
        Just val ->
            Ok val
        Nothing ->
            Err "Key was not found in dict."


maximumBy : (a -> comparable) -> List a -> Result String a
maximumBy fun lst =
    case ListX.maximumBy fun lst of
        Just max ->
            Ok max
        Nothing ->
            Err "failed to get maximum elt from list."


nxtWithArg : (a -> x -> Result e b) -> x -> Result e a -> Result e b
nxtWithArg callback arg result =
    case result of
        Ok value ->
            callback value arg

        Err msg ->
            Err msg


listToString : List Int -> String
listToString lst =
    List.map String.fromInt lst
        |> String.join ", "


arrToString : Array Int -> String
arrToString arr =
    listToString <| Array.toList arr


combineResList : List a -> Result String (List a) -> Result String (List a)
combineResList listA resListB =
    case resListB of
        Ok listB ->
            Ok <| listA ++ listB

        Err e ->
            Err e
