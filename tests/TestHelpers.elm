module TestHelpers exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Matrix exposing (Matrix)


floatEqual : Float -> Float -> Expectation
floatEqual a b =
    Expect.within (Absolute 0.0001) a b


floatEqualPrecise : Float -> Float -> Expectation
floatEqualPrecise a b =
    Expect.within (Absolute 0.000000001) a b


equalMatrices : Matrix -> Matrix -> Expectation
equalMatrices matA matB =
    if matA.nrRows /= matB.nrRows then
        Expect.fail <|
            "Matrices have different number of rows: "
                ++ String.fromInt matA.nrRows
                ++ " and "
                ++ String.fromInt matB.nrRows

    else if matA.nrCols /= matB.nrCols then
        Expect.fail <|
            "Matrices have different number of cols: "
                ++ String.fromInt matA.nrCols
                ++ " and "
                ++ String.fromInt matB.nrCols

    else
        let
            matAList =
                Matrix.flatten matA

            matBList =
                Matrix.flatten matB

            expectList =
                List.map2
                    (\eltA eltB -> Expect.within (Absolute 0.0001) eltA eltB)
                    matAList
                    matBList
        in
        expectAll
            expectList


expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()
