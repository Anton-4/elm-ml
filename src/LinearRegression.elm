module LinearRegression exposing (..)

import Array
import Matrix exposing (Vector)
import Stats exposing (mean, squaredMeanDiffsSum, sum)


type alias LinReg =
    { slope : Float
    , yIntercept : Float
    }


fit : Vector -> Vector -> LinReg
fit x y =
    let
        xMean =
            mean x

        yMean =
            mean y

        ssXX =
            squaredMeanDiffsSum x xMean

        squaredMeanDiffsXY =
            List.map2
                (\xElt yElt ->
                    (xElt - xMean) * (yElt - yMean)
                )
                (Array.toList x)
                (Array.toList y)

        ssXY =
            sum squaredMeanDiffsXY

        slope =
            ssXY / ssXX
    in
    { slope = slope, yIntercept = yMean - slope * xMean }
