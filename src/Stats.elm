module Stats exposing (..)

import Matrix exposing (Vector)
import Array

mean : Vector -> Float
mean vec =
    let
        len = toFloat <| Array.length vec
    in
        sumArr vec / len


squaredMeanDiffsSum : Vector -> Float -> Float
squaredMeanDiffsSum vec vecMean = 
    let
        squaredMeanDiffs = List.map
            (\x -> (vecMean - x) ^ 2)
            <| Array.toList vec
    in
        sum squaredMeanDiffs


std : Vector -> Float
std vec =
    let
        vecMean = mean vec
        len = toFloat <| Array.length vec

        variance = squaredMeanDiffsSum vec vecMean / len
    in
        sqrt variance


sumArr : Vector -> Float
sumArr vec =
    Array.toList vec
        |> List.foldl (+) 0


sum : List Float -> Float
sum lst =
    List.foldl (+) 0 lst
