module Matrix exposing (..)

{-
   Inspired by tortus/elm-array-2d
-}

import Array exposing (Array)
import Helper exposing (listToString, nxt)
import List.Extra
import Maybe exposing (Maybe)
import Maybe.Extra
import Random exposing (Generator)
import Result exposing (Result(..))


type alias Matrix =
    { data : List (List Float)
    , nrRows : Int
    , nrCols : Int
    }


type alias Vector =
    List Float


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


fromList : List Vector -> Result String Matrix
fromList list2D =
    let
        nrRows =
            List.length list2D

        nrColsPerList =
            List.map List.length list2D

        firstListLength =
            List.head nrColsPerList

        rowLengths =
            List.map (\row -> List.length row) list2D
    in
    case firstListLength of
        Just len ->
            if checkListAll (\x -> x == len) rowLengths then
                Ok
                    { data = list2D
                    , nrRows = nrRows
                    , nrCols = len
                    }

            else
                Err <|
                    "Not all rows have the same number of columns. Found row lengths: ("
                        ++ listToString rowLengths
                        ++ ")."

        Nothing ->
            Ok
                { data = list2D
                , nrRows = 1
                , nrCols = 0
                }


fromArray : Array (Array Float) -> Result String Matrix
fromArray arr =
    arr
        |> Array.map Array.toList
        |> Array.toList
        |> fromList


columnMatFromVec : Vector -> Matrix
columnMatFromVec vec =
    let
        list2D =
            List.map (\elt -> [ elt ]) vec
    in
    { data = list2D, nrRows = List.length vec, nrCols = 1 }


empty : Matrix
empty =
    { data = [ [] ]
    , nrRows = 1
    , nrCols = 0
    }


isEmpty : Matrix -> Bool
isEmpty mat =
    mat.nrRows == 0 || mat.nrCols == 0


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
        Ok empty

    else
        Err <| "identityMatrix n: n should be >= 0, but is " ++ String.fromInt n ++ "."


randMatrix : Int -> Int -> Generator Float -> Generator (Result String Matrix)
randMatrix nrRows nrCols floatGen =
    let
        list2DGen =
            Random.list nrRows (Random.list nrCols floatGen)
    in
    Random.map (\lst -> fromList lst) list2DGen


getCol : Matrix -> Int -> Result String Vector
getCol mat colNr =
    case List.Extra.getAt colNr mat.data of
        Just arr ->
            Ok arr

        Nothing ->
            Err <| "colNr " ++ String.fromInt colNr ++ " was out of bounds for matrix with " ++ String.fromInt mat.nrCols ++ " cols."


transformCol : Matrix -> Int -> (Vector -> Result String Vector) -> Result String Matrix
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


updateCol : Matrix -> Int -> Vector -> Result String Matrix
updateCol mat colNr newCol =
    let
        updateMatrix =
            \newMatCol ->
                Ok
                    { data = List.Extra.setAt colNr newMatCol mat.data
                    , nrRows = mat.nrRows
                    , nrCols = mat.nrCols
                    }

        colLength =
            List.length newCol
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
    { matrix | data = List.map (\row -> List.map fun row) matrix.data }


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
                    matrix.data

                transposedList =
                    zip2D dataLists
            in
            { data = transposedList, nrRows = matrix.nrCols, nrCols = matrix.nrRows }


dotProduct : Vector -> Vector -> Float
dotProduct vecA vecB =
    List.foldl (+) 0 <| List.map2 (*) vecA vecB


mul : Matrix -> Matrix -> Result String Matrix
mul matA matB =
    if matA.nrCols /= matB.nrRows then
        Err <|
            "mul: matA must have same number of cols as matB has rows. matA has "
                ++ String.fromInt matA.nrCols
                ++ " cols, matB has "
                ++ String.fromInt matB.nrRows
                ++ " rows."

    else
        let
            mulList =
                List.map
                    (\row ->
                        let
                            transpB =
                                transpose matB

                            products =
                                List.map
                                    (\col -> dotProduct row col)
                                    transpB.data
                        in
                        products
                    )
                    matA.data
        in
        fromList mulList


mulVecWithMat : Vector -> Matrix -> Result String Vector
mulVecWithMat vec mat =
    let
        mat1Dim =
            fromList [ vec ]

        matrixProd =
            mat1Dim
                |> nxt (\mat1D -> mul mat1D mat)
    in
    case matrixProd of
        Ok matProd ->
            let
                firsRow =
                    List.head matProd.data
            in
            case firsRow of
                Just row ->
                    Ok row

                Nothing ->
                    Err "failed to get first row of matrix with Array.get"

        Err e ->
            Err e


mulMatWithVec : Matrix -> Vector -> Result String Vector
mulMatWithVec mat vec =
    let
        mat1Dim =
            fromList [ vec ]

        matrixProd =
            mat1Dim
                |> nxt (\mat1D -> mul mat1D mat)
    in
    case matrixProd of
        Ok matProd ->
            let
                firsRow =
                    List.head matProd.data
            in
            case firsRow of
                Just row ->
                    Ok row

                Nothing ->
                    Err "failed to get first row of matrix with Array.get"

        Err e ->
            Err e


map2 : (Float -> Float -> Float) -> Matrix -> Matrix -> Result String Matrix
map2 fun matA matB =
    let
        flatMatA =
            flatten matA

        flatMatB =
            flatten matB

        equalDims =
            List.length flatMatA == List.length flatMatB
    in
    if equalDims then
        let
            afterFunList =
                List.map2 fun flatMatA flatMatB

            resultMat =
                unflatten matA.nrRows matA.nrCols afterFunList
        in
        resultMat

    else
        Err <|
            "map2: matrices did not have the same dimensions: "
                ++ dims matA
                ++ " and "
                ++ dims matB


flatten : Matrix -> List Float
flatten mat =
    flattenHelper mat.data


flattenHelper : List (List Float) -> List Float
flattenHelper list2D =
    case list2D of
        [] ->
            []

        x :: xs ->
            x ++ flattenHelper xs


unflatten : Int -> Int -> List Float -> Result String Matrix
unflatten nrRows nrCols list =
    let
        list2D =
            unflattenHelper nrCols list

        matRes =
            fromList list2D
    in
    case matRes of
        Ok mat ->
            if mat.nrRows == nrRows && mat.nrCols == nrCols then
                Ok mat

            else
                Err <|
                    "unflatten: list argument did not generate matrix with dims "
                        ++ String.fromInt nrRows
                        ++ "x"
                        ++ String.fromInt nrCols
                        ++ " "
                        ++ ", got "
                        ++ dims mat

        Err e ->
            Err e


unflattenHelper : Int -> List Float -> List (List Float)
unflattenHelper nrCols list =
    case list of
        [] ->
            []

        _ ->
            let
                ( beforeList, afterList ) =
                    split nrCols list
            in
            beforeList :: unflattenHelper nrCols afterList


split : Int -> List a -> ( List a, List a )
split splitIndex list =
    splitHelper splitIndex 0 [] list


splitHelper : Int -> Int -> List a -> List a -> ( List a, List a )
splitHelper splitIndex currentIndex listBefore listAfter =
    if splitIndex == currentIndex then
        ( listBefore, listAfter )

    else
        case listAfter of
            [] ->
                ( listBefore, [] )

            x :: xs ->
                splitHelper
                    splitIndex
                    (currentIndex + 1)
                    (listBefore ++ [ x ])
                    xs


dims : Matrix -> String
dims mat =
    let
        rowStr =
            String.fromInt mat.nrRows

        colStr =
            String.fromInt mat.nrCols
    in
    rowStr ++ "x" ++ colStr


min : Matrix -> Matrix -> Result String Matrix
min matA matB =
    let
        res =
            map2 (-) matA matB
    in
    case res of
        Ok diff ->
            Ok diff

        Err e ->
            Err <| "diff: " ++ e


plus : Matrix -> Matrix -> Result String Matrix
plus matA matB =
    let
        res =
            map2 (+) matA matB
    in
    case res of
        Ok sum ->
            Ok sum

        Err e ->
            Err <| "plus: " ++ e


eltwiseMul : Matrix -> Matrix -> Result String Matrix
eltwiseMul matA matB =
    let
        res =
            map2 (*) matA matB
    in
    case res of
        Ok prod ->
            Ok prod

        Err e ->
            Err <| "eltWiseMul: " ++ e


get : Int -> Int -> Matrix -> Maybe Float
get row col mat =
    List.Extra.getAt row mat.data
        |> Maybe.andThen (List.Extra.getAt col)
