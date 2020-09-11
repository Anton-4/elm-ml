module DataFrame exposing (DataFrame, getCol, indexOfCol, newDF, transformCol, updateCol)

import Helper exposing (nxt)
import List
import List.Extra exposing (elemIndex)
import Matrix exposing (Matrix, Vector)


type alias DataFrame =
    { header : List String
    , values : Matrix
    }


newDF : List String -> Matrix -> Result String DataFrame
newDF header values =
    if List.length header == values.nrCols then
        Ok <| DataFrame header values

    else
        Err "header length does not match nr of cols in matrix"


indexOf : String -> List String -> Result String Int
indexOf elt lst =
    case elemIndex elt lst of
        Just index ->
            Ok index

        Nothing ->
            Err <| "Could not find element " ++ elt ++ " in list " ++ String.join "," lst


indexOfCol : String -> DataFrame -> Result String Int
indexOfCol colName df =
    indexOf colName df.header


getCol : DataFrame -> String -> Result String Vector
getCol df colName =
    let
        colFun =
            \colNr -> Matrix.getCol colNr df.values 
    in
    indexOfCol colName df
        |> nxt colFun


updateCol : DataFrame -> String -> Vector -> Result String DataFrame
updateCol df colName newCol =
    let
        colIndexRes =
            indexOfCol colName df

        updateMatrixFun =
            \colNr -> Matrix.updateCol colNr newCol df.values

        insertMatrixIntoDFFun =
            \newMatrix ->
                Ok
                    { header = df.header
                    , values = newMatrix
                    }
    in
    colIndexRes
        |> nxt updateMatrixFun
        |> nxt insertMatrixIntoDFFun


transformCol : String -> (Vector -> Result String Vector) -> DataFrame -> Result String DataFrame
transformCol colName transFun df =
    let
        colIndexRes =
            indexOfCol colName df 

        colFun =
            \colNr -> Matrix.transformCol colNr transFun df.values

        matrixFun =
            \updatedMatrix ->
                Ok
                    { header = df.header
                    , values = updatedMatrix
                    }
    in
    colIndexRes
        |> nxt colFun
        |> nxt matrixFun
