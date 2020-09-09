module Trees.DecisionTree exposing (..)

import Dict exposing (Dict)
import Dict.Extra exposing (groupBy)
import Matrix exposing (Matrix)
import Stats
import List.Extra exposing (groupWhile)
import Helper exposing (chkNxt, chkM)


--TODO implement for continuous variables
countCategoricalGroups : Int -> Matrix -> Result String (Dict Float Int)
countCategoricalGroups colIndx mat =
    let
        colRes = Matrix.getCol mat colIndx
    in
        case colRes of
            Ok colVec ->
                let
                    groups = 
                        groupBy identity colVec
                    groupsWithCount =
                        Dict.map
                            (\_ v -> List.length v)
                            groups
                in
                Ok <| groupsWithCount
            Err e ->
                Err <| "countCategoricalGroups: " ++ e
            
        
nodeEntropy : Int -> Matrix -> Result String Float
nodeEntropy colIndx mat =
    let
        groupsWithCountRes = countCategoricalGroups colIndx mat
    in
        case groupsWithCountRes of
            Ok groupsWithCount ->
                let
                    groupsWithCountTups = Dict.toList groupsWithCount
                    groupCounts = List.map
                                    (\(_, groupCount) -> toFloat groupCount)
                                    groupsWithCountTups
                    nrRowsFloat = toFloat mat.nrRows
                in
                Ok <| List.foldl
                    (\prevDiff groupCount -> 
                        prevDiff - Stats.entropy groupCount nrRowsFloat
                    )
                    0.0
                    groupCounts

            Err e ->
                Err <| "nodeEntropy: " ++ e


entropyFeatAndLabel : Int -> Int -> Float -> Matrix -> Result String Float
entropyFeatAndLabel featColIndx labelColIndx groupCat mat =
    countCategoricalGroups featColIndx mat
    |> chkNxt "entropyFeatAndLabel"
        (\groupsWithCount ->
            let
                groupCountMaybe = Dict.get groupCat groupsWithCount
            in
                chkM
                    ("Failed to get groupCat: " ++ String.fromFloat groupCat)
                    
        )
    -- let
    --     groupsWithCountRes = countCategoricalGroups featColIndx mat                      
    -- in
    --     case groupsWithCountRes of
    --         Ok groupsWithCount ->
    --             let
    --                 groupCountMaybe = Dict.get groupCat groupsWithCount
    --             in
    --             case groupCountMaybe of
    --                 Just groupCount ->
    --                     Err "TODO"

    --                 Nothing ->
    --                     Err <| "Failed to get groupCat: " ++ String.fromFloat groupCat ++ ""
    --         Err e ->
    --             Err <| "entropyFeatAndLabel: " ++ e
        

