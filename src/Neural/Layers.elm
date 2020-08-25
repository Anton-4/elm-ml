module Neural.Layers exposing (..)

import Matrix exposing (Matrix, randMatrix)
import Neural.Activations exposing (Activation(..), getActFun)
import Random exposing (Generator)
import Random.Float exposing (normal)


type alias LayerConf =
    { layerType : LayerType
    , nrInputNeurons : Int
    , nrOutputNeurons : Int
    , activation : Activation
    }


type alias Layer =
    { layerConf : LayerConf
    , weights : Matrix
    , lastLinearSum : Matrix
    , lastOutput : Matrix
    }


type LayerType
    = Dense


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
                    Ok
                        { layerConf = layerConf
                        , weights = weights
                        , lastLinearSum = Matrix.empty
                        , lastOutput = Matrix.empty
                        }

                Err e ->
                    Err e
        )
        weightsGen


genLayers : Random.Seed -> List LayerConf -> List (Result String Layer)
genLayers seed layerConfs =
    case layerConfs of
        layerConf :: xs ->
            let
                ( layer, nextSeed ) =
                    Random.step (initLayer layerConf) seed
            in
            layer
                :: genLayers nextSeed xs

        [] ->
            []


dense : Int -> Int -> Activation -> LayerConf
dense nrInputNeurons nrOutputNeurons activation =
    { layerType = Dense
    , nrInputNeurons = nrInputNeurons
    , nrOutputNeurons = nrOutputNeurons
    , activation = activation
    }


forwardLayer : Layer -> Matrix -> Result String Layer
forwardLayer layer inputMat =
    let
        forwardMul =
            Matrix.mul inputMat layer.weights
    in
    case forwardMul of
        Ok mat ->
            let
                actFun =
                    getActFun layer.layerConf.activation

                throughActivation =
                    Matrix.map actFun mat
            in
            Ok { layer | lastLinearSum = mat, lastOutput = throughActivation }

        Err e ->
            Err <| "forwardLayer; " ++ e


layerToStr : Layer -> String
layerToStr layer =
    let
        layerConf =
            layer.layerConf
    in
    "Layer: "
        ++ layerTypeToStr layerConf.layerType
        ++ " in: "
        ++ String.fromInt layerConf.nrInputNeurons
        ++ " out: "
        ++ String.fromInt layerConf.nrOutputNeurons
        ++ "actFun: "
        ++ activationToStr layerConf.activation


layerTypeToStr : LayerType -> String
layerTypeToStr layerType =
    case layerType of
        Dense ->
            "Dense"


activationToStr : Activation -> String
activationToStr actFun =
    case actFun of
        Tanh ->
            "Tanh"

        Sigmoid ->
            "Sigmoid"
