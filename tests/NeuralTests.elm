module NeuralTests exposing (denseNetTrain, neuralNetInit)

import Array
import Expect
import List.Extra exposing (last)
import Neural.Activations exposing (Activation(..))
import Neural.Layers exposing (LayerType(..), dense)
import Neural.Net exposing (NeuralNet, initNet)
import Random
import Test exposing (Test, describe, test)
import TestHelpers exposing (floatEqualPrecise)


seed0 : Random.Seed
seed0 =
    Random.initialSeed 42


denseNet1Res : Result String NeuralNet
denseNet1Res =
    initNet seed0
        [ dense 2 4 Sigmoid
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
        denseNet1 =
            case denseNet1Res of
                Ok nn ->
                    nn

                Err e ->
                    []
    in
    describe "dense net training"
        [ test "single forward pass" <|
            \_ ->
                let
                    forwardNNRes =
                        Neural.Net.forward denseNet1 (Array.fromList [ 0.1, -0.1 ])
                in
                case forwardNNRes of
                    Ok forwardNN ->
                        case last forwardNN of
                            Just layerRes ->
                                case layerRes of
                                    Ok layer ->
                                        case Array.get 0 layer.lastForward of
                                            Just output ->
                                                floatEqualPrecise output 0.5008823684026859

                                            Nothing ->
                                                Expect.fail "failed to get first element of last layer lastForward"

                                    Err e ->
                                        Expect.fail e

                            Nothing ->
                                Expect.fail "failed to get last element of forwardNN layer list"

                    Err e ->
                        Expect.fail e
        ]
