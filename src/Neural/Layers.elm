module Neural.Layers exposing (..)

import Matrix exposing (Matrix)
import Neural.Activations exposing(Activation(..))
import Matrix exposing (Matrix, Vector)
import Array

type alias Layer =
    {   layerType : LayerType
        , nrInputNeurons : Int 
        , nrOutputNeurons : Int
        , activation : Activation
        , weights : Matrix
        , lastForward : Vector
    }
    

type LayerType =
    Dense


initWeights : Int -> Int -> Result String Matrix
initWeights nrInput nrOutput =
    Matrix.fromList [[]]


forwardLayer : Layer -> Vector -> Result String Layer
forwardLayer layer inputVec =
    let
        forwardMul =
            Matrix.mulWithVec inputVec layer.weights
    in  
        case forwardMul of
            Ok vec ->
                Ok { layer | lastForward = vec }
            
            Err e ->
                Err <| "forwardLayer; " ++ e


layerToStr : Layer -> String
layerToStr layer =
    "Layer: " ++
    layerTypeToStr layer.layerType ++ " in: " ++
    String.fromInt layer.nrInputNeurons ++ " out: " ++
    String.fromInt layer.nrOutputNeurons ++ "actFun: " ++
    activationToStr layer.activation


layerTypeToStr : LayerType -> String
layerTypeToStr layerType =
    case layerType of
        Dense -> "Dense"

    
activationToStr : Activation -> String
activationToStr actFun =
    case actFun of
        Tanh -> "Tanh"

