module Stats exposing (..)

import Array
import Matrix exposing (Vector)


mean : Vector -> Float
mean vec =
    let
        len =
            toFloat <| List.length vec
    in
    sumArr vec / len


meanList : List Float -> Float
meanList lst =
    let
        len =
            toFloat <| List.length lst
    in
    sum lst / len


squaredMeanDiffsSum : Vector -> Float -> Float
squaredMeanDiffsSum vec vecMean =
    let
        squaredMeanDiffs =
            List.map
                (\x -> (vecMean - x) ^ 2)
                vec
    in
    sum squaredMeanDiffs


std : Vector -> Float
std vec =
    let
        vecMean =
            mean vec

        len =
            toFloat <| List.length vec

        variance =
            squaredMeanDiffsSum vec vecMean / len
    in
    sqrt variance


sumArr : Vector -> Float
sumArr vec =
    List.foldl (+) 0 vec


sum : List Float -> Float
sum lst =
    List.foldl (+) 0 lst
