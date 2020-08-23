module Neural.Net exposing (..)

import Array.Extra exposing (map2)
import Helper exposing (nxt)
import Matrix exposing (Matrix, transpose)
import Neural.Activations exposing (getActFunDer)
import Neural.Layers exposing (Layer, LayerConf, forwardLayer, genLayers, layerToStr)
import Random
import Result.Extra exposing (combine)


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


forward : NeuralNet -> Matrix -> Result String (List (Result String Layer))
forward net inputMat =
    case net of
        [] ->
            Err "forward: Empty neural net given as argument."

        layer :: layers ->
            let
                calcedLayerRes =
                    forwardLayer layer inputMat
            in
            case calcedLayerRes of
                Ok calcedLayer ->
                    case layers of
                        [] ->
                            Ok [ Ok calcedLayer ]

                        _ ->
                            let
                                otherLayersRes =
                                    forward layers calcedLayer.lastOutput
                            in
                            case otherLayersRes of
                                Ok otherLayers ->
                                    Ok (Ok calcedLayer :: otherLayers)

                                Err e ->
                                    Err e

                Err e ->
                    Err <| "forward: layer " ++ layerToStr layer ++ "; " ++ e


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
                        Matrix.mul
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
                            Matrix.emptyMat
                            (List.reverse net)
                    )
    in
    case allWeightChangesRes of
        Ok allWeightChanges ->
            let
                oldWeightsList =
                    List.map (\layer -> layer.weights) net

                updatedWeightsResList =
                    List.map2
                        (\oldWeightsMat weightChangeMat ->
                            Matrix.plus oldWeightsMat weightChangeMat
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
                                                    firstLayer.lastLinearSum
                                                )
                                        )
                        in
                        newDeltaRes
                            |> nxt
                                (\newDelta ->
                                    Matrix.mul
                                        newDelta
                                        (Matrix.transpose inputsMat)
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
                                    (Matrix.transpose prevLayer.lastOutput)
                        in
                        case weightsChangeRes of
                            Ok weightsChange ->
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
