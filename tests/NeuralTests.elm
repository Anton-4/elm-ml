module NeuralTests exposing (neuralNetInit)

import Expect
import Test exposing (Test, describe, test)
import Neural.Net exposing (NeuralNet, initNet)
import Neural.Layers exposing (LayerType(..), dense)
import Neural.Activations exposing (Activation(..))
import Random
import Array

seed0 : Random.Seed
seed0 =
  Random.initialSeed 42


denseNet1Res : Result String NeuralNet
denseNet1Res =
    initNet seed0 [
        dense 2 4 Sigmoid
        , dense 4 1 Sigmoid
    ]


neuralNetInit : Test
neuralNetInit =
    describe "neural net initialization"
        [ test "denseNet1" <|
            \_ -> Expect.ok denseNet1Res
        ]


denseNetTrain : Test
denseNetTrain =
    let
        denseNet1 =  case denseNet1Res of
            Ok nn -> nn
            Err e -> []
    in 
        describe "dense net training"
            [ test "single forward pass" <|
                \_ -> Expect.equal 
                        Neural.Net.forward denseNet1 (Array.fromList [0.1, -0.1])
            ]


