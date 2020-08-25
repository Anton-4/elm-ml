module Neural.Net exposing (..)

import Array.Extra exposing (map2)
import Browser.Navigation exposing (forward)
import Helper exposing (nxt)
import List.Extra
import Matrix exposing (Matrix, transpose)
import Neural.Activations exposing (getActFunDer)
import Neural.Layers exposing (Layer, LayerConf, forwardLayer, genLayers, layerToStr)
import Random
import Result.Extra exposing (combine)
import Stats exposing (mean)


type alias NeuralNet =
    --TODO add cost function
    List Layer


type alias NetConf =
    List LayerConf


initNet : Random.Seed -> NetConf -> Result String NeuralNet
initNet seed netConf =
    let
        layers =
            genLayers seed netConf
    in
    combine layers


forward : Matrix -> NeuralNet -> Result String (List Layer)
forward inputsMat net =
    case net of
        [] ->
            Err "forward: Empty neural net given as argument."

        layer :: layers ->
            let
                calcedLayerRes =
                    forwardLayer layer inputsMat
            in
            case calcedLayerRes of
                Ok calcedLayer ->
                    case layers of
                        [] ->
                            Ok [ calcedLayer ]

                        _ ->
                            let
                                otherLayersRes =
                                    forward calcedLayer.lastOutput layers
                            in
                            case otherLayersRes of
                                Ok otherLayers ->
                                    Ok (calcedLayer :: otherLayers)

                                Err e ->
                                    Err e

                Err e ->
                    Err <| "forward: layer " ++ layerToStr layer ++ "; " ++ e


backward : Matrix -> Matrix -> NeuralNet -> Result String (List Layer)
backward inputsMat labelsMat net =
    let
        lastLayerMaybe =
            List.Extra.last net
    in
    case lastLayerMaybe of
        Just lastLayer ->
            backwardHelper lastLayer inputsMat labelsMat net

        Nothing ->
            Err "backward: failed to get last layer of net, net was probably empty."


backwardHelper : Layer -> Matrix -> Matrix -> NeuralNet -> Result String (List Layer)
backwardHelper lastLayer inputsMat labelsMat net =
    let
        actFunDer =
            getActFunDer lastLayer.layerConf.activation

        costDerivativeRes =
            Matrix.min
                lastLayer.lastOutput
                labelsMat
                |> nxt
                    (\errDiff ->
                        -- let
                        --     _ =
                        --         Debug.log "loss" errDiff
                        -- in
                        Ok
                            (Matrix.map
                                (\d -> d * 2)
                                errDiff
                            )
                    )

        lastDeltaRes =
            costDerivativeRes
                |> nxt
                    (\costDerivative ->
                        Matrix.eltwiseMul
                            costDerivative
                            (Matrix.map
                                actFunDer
                                lastLayer.lastLinearSum
                            )
                    )

        allWeightChangesRes =
            lastDeltaRes
                |> nxt
                    (\lastDelta ->
                        calcWeightChanges
                            inputsMat
                            lastDelta
                            Matrix.empty
                            (List.reverse net)
                    )
    in
    case allWeightChangesRes of
        Ok allWeightChangesRev ->
            let
                allWeightChanges =
                    List.reverse allWeightChangesRev

                oldWeightsList =
                    List.map (\layer -> layer.weights) net

                updatedWeightsResList =
                    List.map2
                        (\oldWeightsMat weightChangeMat ->
                            Matrix.min oldWeightsMat weightChangeMat
                        )
                        oldWeightsList
                        allWeightChanges

                updatedWeightsListRes =
                    Result.Extra.combine updatedWeightsResList
            in
            case updatedWeightsListRes of
                Ok updatedWeightsList ->
                    Ok <|
                        List.map2
                            (\layer newWeights ->
                                { layer | weights = newWeights }
                            )
                            net
                            updatedWeightsList

                Err e ->
                    Err <| "backwardHelper: " ++ e

        Err e ->
            Err <| "backwardHelper: " ++ e


moveResultOut : Result String Matrix -> Result String (List Matrix)
moveResultOut res =
    case res of
        Ok mat ->
            Ok [ mat ]

        Err e ->
            Err e


calcWeightChanges : Matrix -> Matrix -> Matrix -> List Layer -> Result String (List Matrix)
calcWeightChanges inputsMat delta nextWeights revLayers =
    case revLayers of
        [] ->
            Err "calcWeightChanges: received empty list of layers"

        firstLayer :: [] ->
            let
                weightsChangeMatRes =
                    if Matrix.isEmpty nextWeights then
                        -- current layer is last layer
                        Matrix.mul
                            delta
                            (Matrix.transpose inputsMat)

                    else
                        let
                            actFunDer =
                                getActFunDer firstLayer.layerConf.activation

                            deltaWeightsRes =
                                Matrix.mul
                                    nextWeights
                                    delta

                            newDeltaRes =
                                deltaWeightsRes
                                    |> nxt
                                        (\deltaWeights ->
                                            Matrix.eltwiseMul
                                                (transpose deltaWeights)
                                                (Matrix.map
                                                    actFunDer
                                                    firstLayer.lastLinearSum
                                                )
                                        )
                        in
                        newDeltaRes
                            |> nxt
                                (\newDelta ->
                                    Matrix.mul
                                        (transpose newDelta)
                                        inputsMat
                                )
            in
            moveResultOut weightsChangeMatRes

        layer :: prevLayer :: restLayers ->
            let
                weightsChangeDeltaTupRes =
                    if Matrix.isEmpty nextWeights then
                        -- current layer is last layer
                        let
                            weightsChangeRes =
                                Matrix.mul
                                    delta
                                    prevLayer.lastOutput
                        in
                        case weightsChangeRes of
                            Ok weightsChange ->
                                -- let
                                --     _ = Debug.log "delta" delta
                                --     _ = Debug.log "prevLayer.lastOutput" (prevLayer.lastOutput)
                                --     _ = Debug.log "weightsChange" (weightsChange)
                                -- in
                                Ok ( weightsChange, delta )

                            Err e ->
                                Err <| "calcWeightChanges: " ++ e

                    else
                        let
                            actFunDer =
                                getActFunDer layer.layerConf.activation

                            deltaWeightsRes =
                                Matrix.mul
                                    (Matrix.transpose nextWeights)
                                    delta

                            newDeltaRes =
                                deltaWeightsRes
                                    |> nxt
                                        (\deltaWeights ->
                                            Matrix.mul
                                                deltaWeights
                                                (Matrix.map
                                                    actFunDer
                                                    layer.lastLinearSum
                                                )
                                        )

                            weightsChangeMatRes =
                                newDeltaRes
                                    |> nxt
                                        (\newDelta ->
                                            Matrix.mul
                                                newDelta
                                                (Matrix.transpose prevLayer.lastOutput)
                                        )
                        in
                        case ( weightsChangeMatRes, newDeltaRes ) of
                            ( Ok weightsChangeMat, Ok newDelta ) ->
                                Ok ( weightsChangeMat, newDelta )

                            ( Err eWeightChange, Err eNewDelta ) ->
                                Err <| "calcWeightChanges: Both NewDelta and weightsChangeMat calculation failed: \n\t - " ++ eWeightChange ++ "\n\t -" ++ eNewDelta

                            ( Err eWeightChange, Ok _ ) ->
                                Err <| "calcWeightChanges:weightsChangeMat calculation failed (NewDelta succeeded): " ++ eWeightChange

                            ( Ok _, Err eNewDelta ) ->
                                Err <| "calcWeightChanges: NewDelta calculation failed and weightChangeMat succeeded, this should be impossible: " ++ eNewDelta
            in
            case weightsChangeDeltaTupRes of
                Ok ( weightsChangeMat, newDelta ) ->
                    let
                        otherLayersWeightsRes =
                            calcWeightChanges
                                inputsMat
                                newDelta
                                layer.weights
                                (prevLayer :: restLayers)
                    in
                    case otherLayersWeightsRes of
                        Ok otherLayersWeights ->
                            Ok (weightsChangeMat :: otherLayersWeights)

                        Err e ->
                            Err e

                Err e ->
                    Err e


train : Matrix -> Matrix -> Int -> NeuralNet -> Result String NeuralNet
train inputsMat labelsMat epochs net =
    if epochs == 0 then
        Ok net

    else
        let
            fullPassRes =
                forward inputsMat net
                    |> nxt (\fwNet -> backward inputsMat labelsMat fwNet)
        in
        case fullPassRes of
            Ok fullPassNet ->
                train inputsMat labelsMat (epochs - 1) fullPassNet

            Err e ->
                Err <| "train: " ++ e


predictMultiple : Matrix -> NeuralNet -> Result String Matrix
predictMultiple inputsMat net =
    let
        forwardNNRes =
            forward inputsMat net
    in
    case forwardNNRes of
        Ok forwardNN ->
            let
                lastLayerMaybe =
                    List.Extra.last forwardNN
            in
            case lastLayerMaybe of
                Just lastLayer ->
                    Ok lastLayer.lastOutput

                Nothing ->
                    Err "predictMultiple: failed to get last layer of net, net was probably empty."

        Err e ->
            Err <| "predictMultiple: " ++ e


loss : Matrix -> Matrix -> NeuralNet -> Result String Float
loss inputsMat labelsMat net =
    let
        predictRes =
            predictMultiple inputsMat net
    in
    case predictRes of
        Ok predictMat ->
            let
                lossDiffRes =
                    Matrix.min
                        predictMat
                        labelsMat
            in
            case lossDiffRes of
                Ok lossDiffMat ->
                    Matrix.map (\l -> l * l) lossDiffMat
                        |> Matrix.flatten
                        |> Stats.mean
                        |> (\m -> Ok m)

                Err e ->
                    Err <| "loss: " ++ e

        Err e ->
            Err e
