module Neural.Net exposing (..)

import Neural.Layers exposing (Layer, forwardLayer)
import Matrix exposing (Vector, Matrix)
import Array
import Neural.Layers exposing (Layer, LayerConf, forwardLayer, layerToStr, genLayers)
import Random
import Result.Extra exposing (combine)

type alias  NeuralNet =
    List Layer


type alias  NetConf =
    List LayerConf


initNet : Random.Seed -> NetConf -> Result String NeuralNet
initNet seed netConf =
    let
        layers = genLayers seed netConf
    in
        combine layers


forward : NeuralNet -> Vector -> Result String (List (Result String Layer))
forward net inputVec =
    case net of
        [] ->
            Err "forward: Empty neural net given as argument."
        layer :: layers ->
            let
                calcedLayerRes = forwardLayer layer inputVec
            in
                case calcedLayerRes of
                    Ok calcedLayer ->

                        case layers of
                            [] -> Ok [Ok calcedLayer]
                            _ ->
                                let
                                    otherLayersRes = forward layers calcedLayer.lastForward
                                in
                                    case otherLayersRes of
                                        Ok otherLayers ->
                                            Ok ( Ok calcedLayer :: otherLayers)
                                        Err e ->
                                            Err e
                                            
                    Err e ->
                        Err <| "forward: layer " ++ layerToStr layer ++ "; " ++ e

        


backward : NeuralNet -> Vector -> Vector -> NeuralNet
backward net outputVec labelVec =
    []