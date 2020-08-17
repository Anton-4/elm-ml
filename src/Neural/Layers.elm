module Neural.Layers exposing (..)

import Matrix exposing (Matrix)
import Neural.Activations exposing(Activation(..))
import Matrix exposing (Matrix, Vector, randMatrix)
import Random exposing (Generator)
import Random.Float exposing (normal)
import Array
import Random



type alias LayerConf =
    {
        layerType : LayerType
        , nrInputNeurons : Int 
        , nrOutputNeurons : Int
        , activation : Activation
    }
type alias Layer =
    {   
        layerConf : LayerConf
        , weights : Matrix
        , lastForward : Vector
    }
    

type LayerType =
    Dense


initWeights : Int -> Int -> Generator (Result String Matrix)
initWeights nrInput nrOutput =
    randMatrix nrInput nrOutput (normal 0 0.01)


initLayer : LayerConf -> Generator (Result String Layer)
initLayer layerConf =
    let
        weightsGen =
            initWeights
                layerConf.nrInputNeurons
                layerConf.nrOutputNeurons
    in
        Random.map
            (\weightsRes ->
                case weightsRes of
                    Ok weights ->
                        Ok { layerConf = layerConf
                        , weights = weights
                        , lastForward = Array.empty }
                    Err e ->
                        Err e
            )
            weightsGen


genLayers : Random.Seed -> List(LayerConf) -> List (Result String Layer)
genLayers seed layerConfs =
    case layerConfs of
        layerConf :: xs ->
            let
                (layer, nextSeed) = Random.step (initLayer layerConf) seed
            in
                layer ::
                    genLayers nextSeed xs

        [] -> []

dense : Int -> Int -> Activation -> LayerConf
dense nrInputNeurons nrOutputNeurons activation =
    {
        layerType = Dense
        , nrInputNeurons = nrInputNeurons
        , nrOutputNeurons = nrOutputNeurons
        , activation = activation
    }

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
    let
        layerConf = layer.layerConf
    in
        "Layer: " ++
        layerTypeToStr layerConf.layerType ++ " in: " ++
        String.fromInt layerConf.nrInputNeurons ++ " out: " ++
        String.fromInt layerConf.nrOutputNeurons ++ "actFun: " ++
        activationToStr layerConf.activation


layerTypeToStr : LayerType -> String
layerTypeToStr layerType =
    case layerType of
        Dense -> "Dense"

    
activationToStr : Activation -> String
activationToStr actFun =
    case actFun of
        Tanh -> "Tanh"
        Sigmoid -> "Sigmoid"

