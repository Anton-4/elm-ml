module MatrixTests exposing (create, multiplication, transpose)

import Array exposing (Array)
import Expect exposing (Expectation)
import List
import Matrix exposing (fromList, identityMat, mul, mulMatWithVec, mulVecWithMat)
import Test exposing (Test, describe, test)


matrixList1 : List (List Float)
matrixList1 =
    [ [ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ] ]


matrixList1Transpose =
    [ [ 0, 3, 6 ], [ 1, 4, 7 ], [ 2, 5, 8 ] ]


matList1Prod =
    [ [ 15, 18, 21 ], [ 42, 54, 66 ], [ 69, 90, 111 ] ]


matrixList2 : List (List Float)
matrixList2 =
    [ [ 0, 1 ], [ 3, 4 ], [ 6, 7 ] ]


matrixList2Transpose =
    [ [ 0, 3, 6 ], [ 1, 4, 7 ] ]


matrixList3 : List (List Float)
matrixList3 =
    [ [ 0, 1, 2 ], [ 3, 4, 5 ] ]


matList23Prod =
    [ [ 3, 4, 5 ], [ 12, 19, 26 ], [ 21, 34, 47 ] ]


matrixList4 : List (List Float)
matrixList4 =
    [ [ 0, 1, 2 ], [ 3, 4 ] ]


matrixList5 : List (List Float)
matrixList5 =
    [ [ 0, 1, 2 ] ]


matrixList5Transpose =
    [ [ 0 ], [ 1 ], [ 2 ] ]


vecList1 : List Float
vecList1 =
    [ 0, 0, 0 ]


vecList2 : List Float
vecList2 =
    [ 1, 2, 3 ]


vecList3 : List Float
vecList3 =
    [ -2, 2, -3 ]


toArr : List (List Float) -> Array (Array Float)
toArr matrixList =
    Array.fromList <| List.map Array.fromList matrixList


create : Test
create =
    describe "create"
        [ test "3x3 matrix " <|
            \_ ->
                fromList matrixList1 |> Expect.equal (Ok { data = matrixList1, nrRows = 3, nrCols = 3 })
        , test "3x2 matrix " <|
            \_ ->
                fromList matrixList2 |> Expect.equal (Ok { data = matrixList2, nrRows = 3, nrCols = 2 })
        , test "2x3 matrix " <|
            \_ ->
                fromList matrixList3 |> Expect.equal (Ok { data = matrixList3, nrRows = 2, nrCols = 3 })
        , test "empty matrix " <|
            \_ ->
                fromList [ [] ] |> Expect.equal (Ok { data = [ [] ], nrRows = 1, nrCols = 0 })
        , test "empty identity matrix " <|
            \_ ->
                identityMat 0 |> Expect.equal (Ok { data = [ [] ], nrRows = 1, nrCols = 0 })
        , test "single row identity matrix " <|
            \_ ->
                identityMat 1 |> Expect.equal (Ok { data = [ [ 1 ] ], nrRows = 1, nrCols = 1 })
        , test "tow 2 row identity matrix " <|
            \_ ->
                identityMat 2 |> Expect.equal (Ok { data = [ [ 1, 0 ], [ 0, 1 ] ], nrRows = 2, nrCols = 2 })
        , test "5 row identity matrix " <|
            \_ ->
                identityMat 5
                    |> Expect.equal
                        (Ok
                            { data =
                                [ [ 1, 0, 0, 0, 0 ]
                                , [ 0, 1, 0, 0, 0 ]
                                , [ 0, 0, 1, 0, 0 ]
                                , [ 0, 0, 0, 1, 0 ]
                                , [ 0, 0, 0, 0, 1 ]
                                ]
                            , nrRows = 5
                            , nrCols = 5
                            }
                        )
        , test "invalid identity matrix " <|
            \_ ->
                identityMat -3 |> Expect.equal (Err "identityMatrix n: n should be >= 0, but is -3.")
        , test "invalid matrix " <|
            \_ ->
                fromList matrixList4 |> Expect.equal (Err "Not all rows have the same number of columns. Found row lengths: (3, 2).")
        ]


transposeExpect : List (List Float) -> List (List Float) -> Expectation
transposeExpect matrixList expectedTranspose =
    let
        matrixResult =
            Matrix.fromList matrixList
    in
    case matrixResult of
        Ok matrix ->
            let
                transposedMatrix =
                    Matrix.transpose matrix

                expectedTransposeMatrixResult =
                    Matrix.fromList expectedTranspose
            in
            case expectedTransposeMatrixResult of
                Ok expectedTransposeMatrix ->
                    Expect.equal transposedMatrix expectedTransposeMatrix

                Err msg ->
                    Expect.fail msg

        Err msg ->
            Expect.fail msg


transpose : Test
transpose =
    describe "transpose"
        [ test "3x3 matrix" <|
            \_ ->
                transposeExpect matrixList1 matrixList1Transpose
        , test "3x2 matrix" <|
            \_ ->
                transposeExpect matrixList2 matrixList2Transpose
        , test "1x3 matrix" <|
            \_ ->
                transposeExpect matrixList5 matrixList5Transpose
        , test "3x1 matrix" <|
            \_ ->
                transposeExpect matrixList5Transpose matrixList5
        , test "empty matrix" <|
            \_ ->
                transposeExpect [ [] ] [ [] ]
        ]


multiplyExpect : List (List Float) -> List (List Float) -> List (List Float) -> Expectation
multiplyExpect matListA matListB matListC =
    let
        matARes =
            Matrix.fromList matListA

        matBRes =
            Matrix.fromList matListB

        matCRes =
            Matrix.fromList matListC
    in
    case ( matARes, matBRes, matCRes ) of
        ( Ok matA, Ok matB, Ok _ ) ->
            let
                productMat =
                    mul matA matB
            in
            Expect.equal productMat matCRes

        _ ->
            Expect.fail "failed to construct matrix from List"


multiplyMatWithVecExpect : List (List Float) -> List Float -> List Float -> Expectation
multiplyMatWithVecExpect matList vecList vecListProduct =
    let
        matARes =
            Matrix.fromList matList
    in
    case matARes of
        Ok mat ->
            let
                productVec =
                    mulMatWithVec
                        mat
                        vecList
            in
            Expect.equal
                productVec
            <|
                Ok vecListProduct

        _ ->
            Expect.fail "failed to construct matrix from List"


multiplyVecWithMatExpect : List Float -> List (List Float) -> List Float -> Expectation
multiplyVecWithMatExpect vecList matList vecListProduct =
    let
        matARes =
            Matrix.fromList matList
    in
    case matARes of
        Ok mat ->
            let
                productVec =
                    mulVecWithMat
                        vecList
                        mat
            in
            Expect.equal
                productVec
            <|
                Ok vecListProduct

        _ ->
            Expect.fail "failed to construct matrix from List"


multiplication : Test
multiplication =
    describe "multiplication"
        [ test "two 3x3 matrix" <|
            \_ -> multiplyExpect matrixList1 matrixList1 matList1Prod
        , test "3x2 * 2x3 matrix" <|
            \_ -> multiplyExpect matrixList2 matrixList3 matList23Prod
        , test "1x3 * 3x1 matrix" <|
            \_ -> multiplyExpect matrixList5 matrixList5Transpose [ [ 5 ] ]
        , test "1x3 vec * 3x3 matrix" <|
            \_ -> multiplyVecWithMatExpect vecList1 matrixList1 [ 0, 0, 0 ]
        , test "1x3 vec * 3x1 matrix" <|
            \_ -> multiplyVecWithMatExpect vecList1 matrixList5Transpose [ 0 ]
        ]
