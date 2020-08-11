module Matrix exposing (..)

{-
   Inspired by tortus/elm-array-2d
-}

import Array exposing (Array)
import Array.Extra
import Helper exposing (arrToString, nxt)
import Maybe.Extra


type alias Matrix =
    { data : Array (Array Float)
    , nrRows : Int
    , nrCols : Int
    }


type alias Column =
    Array Float


checkListAll : (Int -> Bool) -> List Int -> Bool
checkListAll f lst =
    case lst of
        x :: xs ->
            if not <| f x then
                False

            else
                checkListAll f xs

        [] ->
            True


checkArrAll : (Int -> Bool) -> Array Int -> Bool
checkArrAll f arr =
    checkListAll f <| Array.toList arr


fromArray : Array Column -> Result String Matrix
fromArray array =
    let
        nrRows =
            Array.length array

        nrColsPerArr =
            Array.map Array.length array

        firstArrLength =
            Array.get 0 nrColsPerArr

        rowLengths =
            Array.map (\row -> Array.length row) array
    in
    case firstArrLength of
        Just len ->
            if checkArrAll (\x -> x == len) rowLengths then
                Ok
                    { data = array
                    , nrRows = nrRows
                    , nrCols = len
                    }

            else
                Err <|
                    "Not all rows have the same number of columns. Found row lengths: ("
                        ++ arrToString rowLengths
                        ++ ")."

        Nothing ->
            Ok
                { data = array
                , nrRows = 1
                , nrCols = 0
                }


fromList : List (List Float) -> Result String Matrix
fromList list =
    list
        |> List.map Array.fromList
        |> Array.fromList
        |> fromArray


emptyMat : Matrix
emptyMat =
    { data = Array.fromList [ Array.empty ]
    , nrRows = 1
    , nrCols = 0
    }


identityMat : Int -> Result String Matrix
identityMat n =
    if n > 0 then
        let
            totalRows =
                n - 1

            dataList =
                List.map
                    (\rowNr ->
                        let
                            zerosBefore =
                                List.repeat rowNr 0

                            zerosAfter =
                                List.repeat (totalRows - rowNr) 0

                            row =
                                zerosBefore ++ 1 :: zerosAfter
                        in
                        row
                    )
                    (List.range 0 totalRows)
        in
        dataList |> fromList

    else if n == 0 then
        Ok emptyMat

    else
        Err <| "identityMatrix n: n should be >= 0, but is " ++ String.fromInt n ++ "."


getCol : Matrix -> Int -> Result String Column
getCol mat colNr =
    case Array.get colNr mat.data of
        Just arr ->
            Ok arr

        Nothing ->
            Err <| "colNr " ++ String.fromInt colNr ++ " was out of bounds for matrix with " ++ String.fromInt mat.nrCols ++ " cols."


transformCol : Matrix -> Int -> (Column -> Result String Column) -> Result String Matrix
transformCol mat colNr transFun =
    let
        colRes =
            getCol mat colNr

        updateMatrix =
            \newCol -> updateCol mat colNr newCol
    in
    colRes
        |> nxt transFun
        |> nxt updateMatrix


updateCol : Matrix -> Int -> Column -> Result String Matrix
updateCol mat colNr newCol =
    let
        updateMatrix =
            \newMatCol ->
                Ok
                    { data = Array.set colNr newMatCol mat.data
                    , nrRows = mat.nrRows
                    , nrCols = mat.nrCols
                    }

        colLength =
            Array.length newCol
    in
    if mat.nrRows == colLength then
        updateMatrix newCol

    else
        Err <|
            "New Column does not have same length ("
                ++ String.fromInt colLength
                ++ ") as number of matrix rows ("
                ++ String.fromInt mat.nrRows
                ++ ")."


map : (Float -> Float) -> Matrix -> Matrix
map fun matrix =
    { matrix | data = Array.map (\row -> Array.map fun row) matrix.data }


transpose : Matrix -> Matrix
transpose matrix =
    case matrix.nrCols of
        0 ->
            matrix

        _ ->
            let
                {- Adapted from hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#transpose -}
                zip2D : List (List Float) -> List (List Float)
                zip2D list2D =
                    case list2D of
                        (x :: xs) :: xss ->
                            let
                                otherHeads =
                                    Maybe.Extra.values <| List.map List.head xss

                                otherTails =
                                    Maybe.Extra.values <| List.map List.tail xss
                            in
                            (x :: otherHeads) :: zip2D (xs :: otherTails)

                        [] :: xss ->
                            zip2D xss

                        [] ->
                            []

                dataLists =
                    Array.toList <| Array.map Array.toList matrix.data

                transposedList =
                    zip2D dataLists

                transposedArr =
                    Array.fromList <| List.map Array.fromList transposedList
            in
            { data = transposedArr, nrRows = matrix.nrCols, nrCols = matrix.nrRows }


dotProduct : Array Float -> Array Float -> Float
dotProduct vecA vecB =
    Array.foldl (+) 0 <| Array.Extra.map2 (*) vecA vecB


mul : Matrix -> Matrix -> Result String Matrix
mul matA matB =
    let
        mulArray =
            Array.map
                (\row ->
                    let
                        transpB =
                            transpose matB

                        products =
                            Array.map (\col -> dotProduct row col) transpB.data
                    in
                    products
                )
                matA.data
    in
    fromArray mulArray
