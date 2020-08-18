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


indexOfCol : DataFrame -> String -> Result String Int
indexOfCol df colName =
    indexOf colName df.header


getCol : DataFrame -> String -> Result String Vector
getCol df colName =
    let
        colFun =
            \colNr -> Matrix.getCol df.values colNr
    in
    indexOfCol df colName
        |> nxt colFun


updateCol : DataFrame -> String -> Vector -> Result String DataFrame
updateCol df colName newCol =
    let
        colIndexRes =
            indexOfCol df colName

        updateMatrixFun =
            \colNr -> Matrix.updateCol df.values colNr newCol

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


transformCol : DataFrame -> String -> (Vector -> Result String Vector) -> Result String DataFrame
transformCol df colName transFun =
    let
        colIndexRes =
            indexOfCol df colName

        colFun =
            \colNr -> Matrix.transformCol df.values colNr transFun

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
