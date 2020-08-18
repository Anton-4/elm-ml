module Neural.Net exposing (..)

import Array
import Array.Extra exposing (map2)
import Matrix exposing (Matrix, Vector)
import Neural.Layers exposing (Layer, LayerConf, forwardLayer, genLayers, layerToStr)
import Random
import Result.Extra exposing (combine)
import Neural.Activations exposing (getActFun)
import Matrix


type alias NeuralNet =
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


forward : NeuralNet -> Vector -> Result String (List (Result String Layer))
forward net inputVec =
    case net of
        [] ->
            Err "forward: Empty neural net given as argument."

        layer :: layers ->
            let
                calcedLayerRes =
                    forwardLayer layer inputVec
            in
            case calcedLayerRes of
                Ok calcedLayer ->
                    case layers of
                        [] ->
                            Ok [ Ok calcedLayer ]

                        _ ->
                            let
                                otherLayersRes =
                                    forward layers calcedLayer.lastForward
                            in
                            case otherLayersRes of
                                Ok otherLayers ->
                                    Ok (Ok calcedLayer :: otherLayers)

                                Err e ->
                                    Err e

                Err e ->
                    Err <| "forward: layer " ++ layerToStr layer ++ "; " ++ e


backwardHelper : NeuralNet -> Layer -> Vector -> Vector -> Result String (List Layer)
backwardHelper net lastLayer outputVec labelVec =
    let
        lossVec = map2 (-) labelVec outputVec
        actFun = getActFun lastLayer.layerConf.activation
        lossDerMat = Matrix.columnMatFromVec <| Array.map (\elt -> actFun elt) lossVec
        -- 2*(self.y - self.output) * sigmoid_derivative(self.output)
        allWeightChangesRes = calcWeightChanges
                                (List.reverse net)
                                lossDerMat
    in
        case allWeightChangesRes of
            Ok allWeightChanges ->
                let
                    oldWeightsList = List.map (\layer -> layer.weights) net 
                    layersWithUpdatedWeights = 
                        List.map2
                            (\oldWeightsMat weightChangeMat ->
                                Matrix.plus oldWeightsMat weightChangeMat)
                            oldWeightsList
                            allWeightChanges
                in
                    Err "TODO"  --Ok <| [layersWithUpdatedWeights] ++ backwardHelper ... 
            Err e ->
                Err <| "backwardHelper: " ++ e

calcWeightChanges : List Layer -> Matrix -> Result String (List Matrix)
calcWeightChanges revLayers lossDerMat =
    case revLayers of
        [] -> Err "calcWeightChanges: received empty list of layers"
        layer :: restLayers ->
            let
                weightsChangeMatRes =
                    Matrix.mul
                        (Matrix.transpose layer.weights)
                        lossDerMat

            in
                case weightsChangeMatRes of
                    Ok weightsChangeMat ->
                        case restLayers of
                            [] -> Ok [weightsChangeMat]
                            _ ->
                                let
                                    weightChangesRestRes =
                                        calcWeightChanges restLayers lossDerMat
                                in
                                case weightChangesRestRes of
                                    Ok weightChangesRest ->
                                        Ok <| weightChangesRest ++ [weightsChangeMat]

                                    Err e ->
                                        Err <| "calcWeightChanges: " ++ e

                    Err e ->
                        Err <| "calcWeightChanges: " ++ e

-- # application of the chain rule to find derivative of the lossDer function with respect to weights2 and weights1
-- d_weights2 = np.dot(self.layer1.T, (2*(self.y - self.output) * sigmoid_derivative(self.output)))
-- d_weights1 = np.dot(self.input.T,  (np.dot(2*(self.y - self.output) * sigmoid_derivative(self.output), self.weights2.T) * sigmoid_derivative(self.layer1)))

-- # update the weights with the derivative (slope) of the lossDer function
-- self.weights1 += d_weights1
-- self.weights2 += d_weights2
