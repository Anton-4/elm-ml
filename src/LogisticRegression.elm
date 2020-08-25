module LogisticRegression exposing (..)

import Array
import Matrix exposing (Vector)


type alias LogReg =
    { weights : Vector
    , bias : Float
    }


fit : Vector -> Vector -> LogReg
fit x y =
    let
        nrFeats = Array.length x
        weights = List.repeat nrFeats 0.0
        bias = 0.0
    in
        { weights= Array.fromList weights, bias= bias}