module LinRegTests exposing (twoDimTest)

import Array
import Expect exposing (FloatingPointTolerance(..))
import Matrix exposing (Vector)
import Test exposing (Test, describe, test)
import TestHelpers exposing (floatEqual)
import LinearRegression exposing (fit)



vecX1 : Vector
vecX1 =
    Array.fromList [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]


vecY1 : Vector
vecY1 =
    Array.fromList [ 1, 3, 2, 5, 7, 8, 8, 9, 10, 12]

twoDimTest : Test
twoDimTest =
    let
        linRegModel = fit vecX1 vecY1
    in
    describe "lin reg with two dimensional data"
        [ test "slope correct" <|
            \_ ->
                floatEqual linRegModel.slope 1.1696970
        , test "yIntercept correct" <|
            \_ ->
                floatEqual linRegModel.yIntercept 1.236364
        ]
