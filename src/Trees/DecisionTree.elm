module Trees.DecisionTree exposing (..)

import Dict exposing (Dict)
import Dict.Extra exposing (groupBy)
import Result.Extra exposing (combine)
import Matrix exposing (Matrix)
import Stats exposing (entropy)
import List.Extra as ListX exposing (groupWhile)
import Maybe.Extra as MaybeX
import Helper exposing (chkNxt, nxt, getRes, maximumBy)
import Set
import Trees.Tree exposing (Node(..))

--TODO implement for continuous variables
countCategoricalGroups : Int -> Matrix -> Result String (Dict Float Float)
countCategoricalGroups colIndx mat =
    let
        colRes = Matrix.getCol colIndx mat
    in
        case colRes of
            Ok colVec ->
                let
                    groups = 
                        groupBy identity colVec
                    groupsWithCount =
                        Dict.map
                            (\_ v -> toFloat <| List.length v)
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
                                    (\(_, groupCount) -> groupCount)
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
    let
        fName = "entropyFeatAndLabel"
    in
        countCategoricalGroups featColIndx mat
            |> chkNxt fName
                (\groupsWithCount ->
                    getRes groupCat groupsWithCount
                    |> nxt (\featGroupCount ->
                        Matrix.getCol featColIndx mat
                        |> nxt (\featCol ->
                            Matrix.getCol labelColIndx mat
                            |> nxt (\labelCol ->
                                let
                                    labelMaybes = List.map2
                                        (\cat label ->
                                            if cat == groupCat then
                                                Just label
                                            else
                                                Nothing
                                        )
                                        featCol
                                        labelCol
                                    labelsLst =
                                        MaybeX.values labelMaybes

                                    labelCounts =
                                        groupBy identity labelsLst
                                        |> Dict.values
                                        |> List.map
                                            (\lst -> toFloat <| List.length lst)

                                    entropyList = List.map
                                        (\labelCount ->
                                            entropy labelCount featGroupCount
                                        )
                                        labelCounts

                                    entropyDiff =
                                        List.foldl (-) 0.0 entropyList
                                in
                                    Ok entropyDiff
                            )
                        )
                    )
                )
    
        
calcGain : Int -> Int -> Float -> Matrix -> Result String Float
calcGain featColIndx labelColIndx parentEntropy mat =
    countCategoricalGroups featColIndx mat
    |> chkNxt "calcGain" (\groupsWithCount ->
                let
                    entropyProbsDict =
                        Dict.map
                            (\cat count ->
                                let
                                    prob = 
                                        count / toFloat mat.nrRows                                        
                                in
                                    entropyFeatAndLabel featColIndx labelColIndx cat mat
                                    |> nxt (\entropy ->
                                        Ok <| prob * entropy
                                    )
                            )
                            groupsWithCount

                    entropyProbsRes =
                        Dict.values entropyProbsDict
                        |> combine

                    gainRes =
                        entropyProbsRes
                        |> nxt (\entropyProbs ->
                            Ok <| List.foldl
                                (-)
                                parentEntropy
                                entropyProbs
                        )  
                in
                    gainRes
                    
            )


constructTree : Node -> List Int -> Int -> Float -> Matrix -> Result String Node
constructTree node nonLabelColsIndxs labelColIndx parentEntropy mat =
    case nonLabelColsIndxs of
        [] -> Ok node
        nonLabelCols ->
            let
                featGainsWRes =
                    List.map
                        (\featIndx ->
                            (featIndx
                            , calcGain featIndx labelColIndx parentEntropy mat)
                        )
                        nonLabelCols

                feats = List.map
                            (\(feat, _) -> feat)
                            featGainsWRes

                gainsResLst = List.map
                            (\(_, gainRes) -> gainRes)
                            featGainsWRes

                gainsRes = combine gainsResLst
            in
                case gainsRes of
                    Ok gains ->
                        let
                            featGains =
                                List.map2
                                    (\feat gain -> (feat, gain))
                                    feats
                                    gains
                        in

                        maximumBy
                            (\(_, gain) -> gain)
                            featGains
                        |> chkNxt "constructTree"
                        (\(maxGainFeatIndx, maxGain) ->
                            Matrix.getCol maxGainFeatIndx mat
                            |> nxt (\maxGainFeatCol -> 
                                let
                                    cats = Set.fromList maxGainFeatCol
                                in
                                    List.map
                                        (\cat ->
                                            let
                                                entropy = entropyFeatAndLabel maxGainFeatIndx labelColIndx cat mat
                                                childNode = Node maxGainFeatIndx []
                                            in
                                                Err "TODO"
                                        )
                                        cats
                                    |> combine
                            )
                        )
                    Err e ->
                        Err <| "constructTree: failed to calculcate gain:" ++ e