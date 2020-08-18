module Matrix exposing (..)

{-
   Inspired by tortus/elm-array-2d
-}

import Array exposing (Array)
import Array.Extra
import Helper exposing (arrToString, nxt)
import Maybe.Extra
import Random exposing (Generator)


type alias Matrix =
    { data : Array (Array Float)
    , nrRows : Int
    , nrCols : Int
    }


type alias Vector =
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


fromArray : Array Vector -> Result String Matrix
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



columnMatFromVec : Vector -> Matrix
columnMatFromVec vec =
    let
        list2D = 
            Array.toList vec
            |> List.map (\elt -> [elt])
    in
        { data = Array.fromList <| List.map Array.fromList list2D, nrRows = Array.length vec, nrCols = 1}


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



-- randMatrix : Int -> Int -> Generator Float -> Result String (Generator Matrix)
-- randMatrix nrRows nrCols foatGen =
--     let
--         rows = List.repeat nrRows []
--         mat = List.map (\_ -> )
--     in
--         Random.list nrCols floatGen


randMatrix : Int -> Int -> Generator Float -> Generator (Result String Matrix)
randMatrix nrRows nrCols floatGen =
    let
        list2DGen =
            Random.list nrRows (Random.list nrCols floatGen)
    in
    Random.map (\lst -> fromList lst) list2DGen


getCol : Matrix -> Int -> Result String Vector
getCol mat colNr =
    case Array.get colNr mat.data of
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


to2DList : Matrix -> List (List Float)
to2DList mat =
    Array.toList <| Array.map Array.toList mat.data


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
                    to2DList matrix

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


mulVecWithMat : Vector -> Matrix -> Result String Vector
mulVecWithMat vec mat =
    let
        mat1Dim =
            fromArray <| Array.fromList [ vec ]

        matrixProd =
            mat1Dim
                |> nxt (\mat1D -> mul mat1D mat)
    in
    case matrixProd of
        Ok matProd ->
            let
                firsRow =
                    Array.get 0 matProd.data
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
            fromArray <| Array.fromList [ vec ]

        matrixProd =
            mat1Dim
                |> nxt (\mat1D -> mul mat1D mat)
    in
    case matrixProd of
        Ok matProd ->
            let
                firsRow =
                    Array.get 0 matProd.data
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
        flatMatA = flatten matA
        flatMatB = flatten matB

        equalDims = List.length flatMatA == List.length flatMatB
    in
        if equalDims then
            let
                afterFunList = List.map2 fun flatMatA flatMatB
                resultMat = unflatten matA.nrRows matA.nrCols afterFunList
            in
                resultMat

        else
            Err <|
                "map2: matrices did not have the same dimensions: " ++
                dims matA ++ " and " ++
                dims matB


flatten : Matrix -> List Float
flatten mat =
    to2DList mat
    |> flattenHelper


flattenHelper : List (List Float) -> List Float
flattenHelper list2D =
    case list2D of
        [] -> []
        x :: xs ->
            x ++ flattenHelper xs


unflatten : Int -> Int -> List Float -> Result String Matrix
unflatten nrRows nrCols list =
    let
        list2D = unflattenHelper nrRows list
        matRes = fromList list2D
    in
        case matRes of
            Ok mat -> 
                if mat.nrRows == nrRows && mat.nrCols == nrCols then
                    Ok mat
                else
                    Err <|
                        "unflatten: list argument did not generate matrix with dims " ++
                        String.fromInt nrRows ++ "x" ++ String.fromInt nrCols ++ " " ++
                        ", got " ++ dims mat
            Err e ->
                Err e
        

unflattenHelper : Int -> List Float -> List (List Float)
unflattenHelper nrCols list =
    case list of
        [] -> []

        _ -> 
            let
                (beforeList, afterList) = split nrCols list
            in
                beforeList :: unflattenHelper nrCols afterList



split : Int -> List a -> (List a, List a)
split splitIndex list =
    splitHelper splitIndex 0 [] list
    

splitHelper : Int -> Int -> List a -> List a -> (List a, List a)
splitHelper splitIndex currentIndex listBefore listAfter =
    if splitIndex == currentIndex then
        (listBefore, listAfter)
    else
        case listAfter of
            [] -> (listBefore, [])
            x :: xs -> 
                splitHelper
                    splitIndex
                    (currentIndex + 1 )
                    (listBefore ++ [x])
                    xs
            

dims : Matrix -> String
dims mat =
    let
        rowStr = String.fromInt mat.nrRows
        colStr = String.fromInt mat.nrCols
    in
        rowStr ++ "x" ++ colStr


min : Matrix -> Matrix -> Result String Matrix
min matA matB =
    map2 (-) matA matB


plus : Matrix -> Matrix -> Result String Matrix
plus matA matB =
    map2 (+) matA matB

